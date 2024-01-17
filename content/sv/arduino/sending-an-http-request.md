---
title:                "Sända en http-förfrågan"
html_title:           "Arduino: Sända en http-förfrågan"
simple_title:         "Sända en http-förfrågan"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/sending-an-http-request.md"
---

{{< edit_this_page >}}

Vad & Varför?
Att skicka en HTTP-förfrågan innebär att skicka en begäran till en webbserver för att hämta data. Det är ett vanligt sätt för programutvecklare att få åtkomst till data från externa källor.

Hur man gör:
```Arduino
#include <WiFiClient.h>
#include <WiFiEsp.h>

char server[] = "www.example.com";  //server som förfrågan ska skickas till
WiFiClient client;  //skapa ett WiFiClient objekt
int status = WL_IDLE_STATUS;  //variabel för att hålla reda på WiFi-status

void setup() {
  Serial.begin(115200);
  WiFi.init(&Serial);
  
  //anslut till WiFi-nätverket
  while (status != WL_CONNECTED) {
    Serial.print("Försöker ansluta till nätverket...");
    status = WiFi.begin("SSID", "lösenord"); //ersätt med ditt nätverks SSID och lösenord
    delay(5000);
  }
  Serial.println("Ansluten till nätverket!");
}

void loop() {
  //ansluta till servern
  if (client.connect(server, 80)) {
    Serial.println("Ansluten till servern!");
    
    //skicka förfrågan
    client.println("GET /data/ HTTP/1.1");
    client.println("Host: www.example.com");
    client.println("Connection: close");
    client.println();
    
    //läs och skriv ut svaret
    while (client.available()) {
      char c = client.read();
      Serial.print(c);
    }
    
    Serial.println("Stäng anslutningen när alla data har tagits emot.");
    client.stop(); //stänga anslutningen när all data har tagits emot
  } 
  else {
    Serial.println("Anslutningen misslyckades :(");
  }
  
  delay(5000); //vänta 5 sekunder innan du skickar en ny förfrågan
}
```

Utforska vidare:
HTTP-förfrågan är en del av HTTP-protokollet, vilket hänvisar till hur data kommuniceras mellan en klient och en server på World Wide Web. Det finns andra sätt att hämta data från externa källor, som t.ex. FTP eller SMTP, men HTTP är det vanligaste och mest lättanvända sättet.

Se även:
- Arduino WiFi bibliotekets dokumentation: https://www.arduino.cc/en/Reference/WiFiClient
- HTTP-protokollets officiella dokumentation: https://www.w3.org/Protocols/rfc2616/rfc2616.html