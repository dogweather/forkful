---
title:                "Arduino: Å sende en http-forespørsel med grunnleggende autentisering"
simple_title:         "Å sende en http-forespørsel med grunnleggende autentisering"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Hvorfor

Det er mange gode grunner til å sende en HTTP-forespørsel med grunnleggende autentisering i Arduino-programmering. Det kan være nødvendig for å kommunisere med en nettskyplattform eller en ekstern server for å hente data eller utføre handlinger.

## Hvordan

For å sende en HTTP-forespørsel med grunnleggende autentisering i Arduino, må vi følge noen få enkle steg.

Først må vi inkludere biblioteket "WiFiClientSecure.h" i koden vår. Dette gir oss tilgang til funksjoner for å opprette en sikker tilkobling med en server.

Deretter må vi opprette en WiFi-klient og en HTTPS-tilkobling til ønsket nettadresse. Deretter kan vi definere forespørselen vår ved å spesifisere metoden (f.eks. GET eller POST), URI og HTTP-versjon. Vi må også legge til en "Authorization" -header med våre autentiseringsdetaljer.

Til slutt kan vi sende forespørselen vår med "client.print()", og lese svaret med "client.read()". Et eksempel på denne koden er som følger:

```Arduino
#include <WiFiClientSecure.h>

const char* ssid = "wifi-nettverk";
const char* password = "passordet ditt";
const char* host = "www.din-nettside.no";
const int httpsPort = 443;

WiFiClientSecure client;

void setup() {
  Serial.begin(115200);
  WiFi.begin(ssid, password);

  while (WiFi.status() != WL_CONNECTED) {
    delay(500);
  }

  Serial.println("WiFi tilkoblet");

  Serial.print("HTTPS-tilkobling til ");
  Serial.println(host);

  if (!client.connect(host, httpsPort)) {
    Serial.println("Kan ikke opprette en sikker tilkobling");
    return;
  }

  Serial.print("Forespørsel sendt til serveren");
  client.println("GET /api/data HTTP/1.1");
  client.println("Host: www.din-nettside.no");
  client.println("Authorization: Basic ditt autentiseringspassord");
  client.println("Connection: close");
  client.println();

  while (client.available()) {
    String line = client.readStringUntil('\r');
    Serial.print(line);
  }

  Serial.println();
  Serial.println("Tilkobling lukket");
}

void loop() {

}
```

Output fra serieporten vil vise svaret fra serveren og bekrefte at forespørselen ble sendt og autentisering ble vellykket.

## Dypdykk

Når vi sender en HTTP-forespørsel med grunnleggende autentisering, sender vi en base64-kodet streng med brukernavnet og passordet vårt i "Authorization" -headeren. Denne kodingen er ikke en form for en sikkerhet, men mer som en enkel måte å gjøre informasjonen vår forståelig for serveren.

Det er også viktig å merke seg at denne metoden for autentisering ikke er like sikker som andre former for autentisering, for eksempel OAuth. Derfor bør den bare brukes når det er nødvendig og når kommunikasjonen med serveren er beskyttet med HTTPS.

## Se også

- Offisiell dokumentasjon for WiFiClientSecure: https://www.arduino.cc/en/Reference/WiFiClientSecure
- Code for Fun sin guide om å sende en HTTP-forespørsel i Arduino: https://www.codeforfun.no/sende-http-foresporsel-med-arduino/
- Tutorial om å bruke grunnleggende autentisering i en Arduino-prosjekt: https://randomnerdtutorials.com/esp32-cam-post-image-photo-server-basic-authentication/