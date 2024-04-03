---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:11:28.327330-07:00
description: "Come fare: L'analisi di HTML su Arduino di solito richiede librerie\
  \ di piccole dimensioni a causa delle limitate risorse del dispositivo. Una scelta\u2026"
lastmod: '2024-03-13T22:44:43.681527-06:00'
model: gpt-4-0125-preview
summary: L'analisi di HTML su Arduino di solito richiede librerie di piccole dimensioni
  a causa delle limitate risorse del dispositivo.
title: Analisi del HTML
weight: 43
---

## Come fare:
L'analisi di HTML su Arduino di solito richiede librerie di piccole dimensioni a causa delle limitate risorse del dispositivo. Una scelta popolare per lo scraping e l'analisi del web consiste nell'usare le librerie `ESP8266HTTPClient` e `ESP8266WiFi` per ESP8266, o i loro equivalenti per ESP32, data la loro supporto nativo per le capacità Wi-Fi e i protocolli HTTP. Ecco un esempio base per recuperare e analizzare HTML, assumendo che stiate lavorando con un ESP8266 o ESP32:

Prima, includi le librerie necessarie:
```cpp
#include <ESP8266WiFi.h> // Per ESP8266
#include <ESP8266HTTPClient.h>
#include <WiFiClient.h>
// Usa le librerie analoghe per ESP32 se stai usando un ESP32

const char* ssid = "tuoSSID";
const char* password = "tuaPASSWORD";
```

Connettiti alla tua rete Wi-Fi:
```cpp
void setup() {
    Serial.begin(115200);
    WiFi.begin(ssid, password);

    while (WiFi.status() != WL_CONNECTED) {
        delay(1000);
        Serial.println("Connessione in corso...");
    }
}
```

Effettua una richiesta HTTP e analizza un semplice pezzo di HTML:
```cpp
void loop() {
    if (WiFi.status() == WL_CONNECTED) { //Controlla lo stato della connessione Wi-Fi
        HTTPClient http;  //Dichiara un oggetto della classe HTTPClient

        http.begin("http://example.com");  //Specifica la destinazione della richiesta
        int httpCode = http.GET();  //Invia la richiesta

        if (httpCode > 0) { //Controlla il codice di ritorno
            String payload = http.getString();   //Ottieni il payload della risposta alla richiesta
            Serial.println(payload);             //Stampa il payload della risposta

            // Analizza una parte specifica, ad es., estraendo il titolo dal payload
            int titleStart = payload.indexOf("<title>") + 7; // +7 per superare il tag "<title>"
            int titleEnd = payload.indexOf("</title>", titleStart);
            String pageTitle = payload.substring(titleStart, titleEnd);

            Serial.print("Titolo Pagina: ");
            Serial.println(pageTitle);
        }

        http.end();   //Chiudi connessione
    }

    delay(10000); //Fai una richiesta ogni 10 secondi
}
```

Esempio di output (assumendo che http://example.com abbia una semplice struttura HTML):
```
Connessione in corso...
...
Titolo Pagina: Example Domain
```

Questo esempio dimostra come recuperare una pagina HTML ed estrarre il contenuto del tag `<title>`. Per analisi HTML più complesse, considera l'uso di espressioni regolari (con cautela a causa delle limitazioni di memoria) o funzioni di manipolazione delle stringhe per navigare attraverso la struttura HTML. L'analisi avanzata potrebbe richiedere approcci più sofisticati, inclusi algoritmi di parsing personalizzati adattati alla struttura specifica dell'HTML con cui si sta lavorando, poiché l'ambiente Arduino standard non include una libreria di parsing HTML integrata.
