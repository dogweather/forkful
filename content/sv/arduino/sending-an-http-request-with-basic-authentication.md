---
title:                "Arduino: Sända en http-begäran med grundläggande autentisering"
simple_title:         "Sända en http-begäran med grundläggande autentisering"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Varför

I denna bloggpost kommer vi att gå igenom hur du kan skicka en HTTP-begäran med grundläggande autentisering genom att använda Arduino-programmering. Detta kan vara användbart för att kommunicera med externa webbtjänster eller för att säkert skicka data över ett nätverk.

## Så här gör du

För att skicka en HTTP-begäran med grundläggande autentisering behöver du först definiera de nödvändiga variablerna för din begäran, inklusive din användar- och lösenordsuppgifter. Dessa variabler kan sedan användas för att skapa en HTTP-begäran med den korrekta autentiseringen. Nedan följer en kodexempel som visar hur du kan göra detta med en Arduino Uno:

```Arduino
// Definiera variabler för webbadress, användarnamn och lösenord
String url = "https://example.com/api/data";
String username = "användare";
String password = "lösenord";

// Skapa en HTTP-klient och ange begäran som GET
WiFiClient client;
if (!client.connect("example.com", 443)) {
  Serial.println("Anslutning misslyckades");
  return;
}
client.println("GET /api/data HTTP/1.1");

// Lägg till headers för autentisering och slutför begäran
String auth = "Authorization: Basic " + base64::encode(username + ":" + password);
client.println(auth);
client.println("Host: example.com");
client.println("Connection: close");
client.println();

// Vänta på svar från servern och skriv ut svaret
while(client.available()) {
  String line = client.readStringUntil('\r');
  Serial.println(line);
}
client.stop();
```

Efter att du har anslutit till webbtjänsten och skickat din begäran, bör du få ett svar från servern som innehåller den efterfrågade informationen.

## Deep Dive

HTTP-protokollet är en viktig del av kommunikationen på internet och används för att skicka data mellan klienter och servrar. En HTTP-begäran består av en begäran från klienten och ett svar från servern. För att autentisera en begäran med grundläggande autentisering måste klienten bifoga en header med användar- och lösenordsuppgifterna i begäran. Servern kommer sedan att kolla om dessa uppgifter matchar de som behövs för att autentisera och svara med den efterfrågade informationen om autentiseringen lyckas.

Det är viktigt att notera att grundläggande autentisering inte är den mest säkra metoden för autentisering. Om möjligt bör andra autentiseringsmetoder användas för att säkerställa att din kommunikation är skyddad.

## Se också

- [Basic Authentication in HTTP](http://www.ietf.org/rfc/rfc2617.txt)
- [WiFiClient Documentation](https://www.arduino.cc/en/Reference/WiFiClient)
- [HTTP Requests with ESP8266](https://randomnerdtutorials.com/esp8266-http-get-request-arduino/)

Tack för att du läste! Om du har frågor eller synpunkter, tveka inte att kontakta oss. Ha det bra!