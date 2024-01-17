---
title:                "Perusautentikoinnin lähettäminen http-pyynnön avulla"
html_title:           "Arduino: Perusautentikoinnin lähettäminen http-pyynnön avulla"
simple_title:         "Perusautentikoinnin lähettäminen http-pyynnön avulla"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Mikä ja miksi?
Lähettämällä HTTP-pyynnön perusautentikoinnin kanssa, voit hakea tietoja verkosta ja suojata niitä samalla salasanalla. Tämä on hyödyllistä, sillä se parantaa tietoturvaa ja antaa sinulle mahdollisuuden saada tietoa ulkopuolisista lähteistä.

## Miten:
Esimerkkejä ohjelmoinnista ja tulosteista ```Arduino ... ``` koodilohkoilla.

Esimerkki hakee tietoja Google Maps API:sta autentikoinnin avulla:

```Arduino
#include <WiFiClient.h>
#include <ESP8266HTTPClient.h>

// Replace with your network credentials
const char *ssid = "YourNetworkName";
const char *password = "YourNetworkPassword";

// Replace with your Google Maps API key
String apiKey = "YourAPIKey";

void setup()
{
  Serial.begin(115200);
  WiFi.begin(ssid, password);

  while (WiFi.status() != WL_CONNECTED)
  {
    delay(1000);
    Serial.println("Connecting to WiFi...");
  }
  Serial.println("Connected to WiFi!");

  // Set up HTTP request
  HTTPClient http;
  http.begin("https://maps.googleapis.com/maps/api/geocode/json?address=1600+Amphitheatre+Parkway,+Mountain+View,+CA&key=" + apiKey);
  http.addHeader("Content-Type", "application/json");

  // Send request with basic authentication
  http.setAuthorization("username", "password");
  int statusCode = http.sendRequest();

  // Check for successful response
  if (statusCode > 0)
  {
    // Read response and print to serial monitor
    String response = http.getString();
    Serial.println(response);
  }
  else
  {
    Serial.println("Error in HTTP request");
  }

  // Close connection
  http.end();
}

void loop()
{
}
```

Tulostus serial monitoriin:

```
HTTP/1.1 200 OK
Content-Type: application/json; charset=UTF-8
Date: Wed, 14 Aug 2019 00:00:00 GMT
Expires: Thu, 15 Aug 2019 00:00:00 GMT
Cache-Control: public, max-age=86400
Server: ESF
Content-Encoding: gzip
X-XSS-Protection: 0
X-Frame-Options: SAMEORIGIN
Alt-Svc: quic=":443"; ma=2592000; v="46,43,39"
Transfer-Encoding: chunked
Connection: close

{
  "results" : [
    {
      "address_components" : [
        {
          "long_name" : "1600",
          "short_name" : "1600",
          "types" : [ "street_number" ]
        },
        {
          "long_name" : "Amphitheatre Parkway",
          "short_name" : "Amphitheatre Pkwy",
          "types" : [ "route" ]
        },
        {
          "long_name" : "Mountain View",
          "short_name" : "Mountain View",
          "types" : [ "locality", "political" ]
        },
        {
          "long_name" : "California",
          "short_name" : "CA",
          "types" : [ "administrative_area_level_1", "political" ]
        },
        {
          "long_name" : "United States",
          "short_name" : "US",
          "types" : [ "country", "political" ]
        },
        {
          "long_name" : "94043",
          "short_name" : "94043",
          "types" : [ "postal_code" ]
        }
      ],
      "formatted_address" : "1600 Amphitheatre Pkwy, Mountain View, CA 94043, USA",
      "geometry" : {
         "location" : {
            "lat" : 37.4219999,
            "lng" : -122.0839589
         },
         "location_type" : "ROOFTOP",
         "viewport" : {
            "northeast" : {
               "lat" : 37.4233488802915,
               "lng" : -122.0826099197085
            },
            "southwest" : {
               "lat" : 37.4206509197085,
               "lng" : -122.0853078802915
            }
         }
      },
      "place_id" : "ChIJ2eUgeAK6j4ARbn5u_wAGqWA",
      "plus_code" : {
         "compound_code" : "CWC8+W5 Mountain View, California",
         "global_code" : "849VCWC8+W5"
      },
      "types" : [ "street_address" ]
    }
  ],
  "status" : "OK"
}
```

## Syvempi sukellus:
Perusautentikointi on yksi vanhimmista ja yksinkertaisimmista tavoista varmistaa tietoturva HTTP-pyynnöissä. Sitä käytetään edelleen monissa sovelluksissa, mutta monet suosivat nyt turvallisempia menetelmiä, kuten Token-autentikointia.

## Katso myös:
Tässä on muutamia linkkejä aiheeseen liittyviin lähteisiin:

- [HTTPClient library in Arduino](https://github.com/esp8266/Arduino/tree/master/libraries/ESP8266HTTPClient)
- [Google Maps API documentation](https://developers.google.com/maps/documentation)
- [Basic Authentication in HTTP](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#Basic_authentication_scheme)