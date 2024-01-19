---
title:                "Envoyer une requête http"
html_title:           "Fish Shell: Envoyer une requête http"
simple_title:         "Envoyer une requête http"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & Pourquoi ?

L'envoi d'une requête HTTP est une méthode permettant de communiquer avec un serveur via internet. Les programmeurs le font pour récupérer ou envoyer des données à un serveur.

## Comment faire:

Voici un exemple de code Arduino pour envoyer une requête HTTP GET:

```Arduino
#include <ESP8266WiFi.h>
#include <ESP8266HTTPClient.h>

const char* ssid = "your_SSID";
const char* password = "your_PASSWORD";

void setup () {
  Serial.begin(115200);
  WiFi.begin(ssid, password);

  while (WiFi.status() != WL_CONNECTED) {
    delay(1000);
    Serial.println("Connexion WiFi...");
  }
}

void loop() {
  if (WiFi.status() == WL_CONNECTED) { 
    HTTPClient http; 

    http.begin("http://example.com"); 
    int httpCode = http.GET(); 

    if (httpCode > 0) { 
      String payload = http.getString();
      Serial.println(payload);  
    }
    http.end(); 
  }
  delay(30000); 
}
```

Lorsque vous exécuterez ce code, il se connectera à votre réseau WiFi et enverra une requête HTTP GET à "http://example.com". Il affichera ensuite la réponse du serveur dans le moniteur série.

## Plongée en profondeur

L'envoi de requêtes HTTP a vu le jour avec le début du World Wide Web en 1989. Avant l'existence des API RESTful et des requêtes HTTP, il était plus compliqué d'accéder et de manipuler des données sur le Web. 

De nos jours, l'envoi de requêtes HTTP est l'une des principales manières d'interagir avec les API RESTful, même si d'autres méthodes existent, comme les sockets Web et GraphQL. Cependant, les requêtes HTTP sont généralement plus universellement prises en charge et plus simples à utiliser.

En ce qui concerne les détails de mise en œuvre, cet exemple est basé sur l'utilisation du module ESP8266. Si vous utilisez un autre module WiFi avec votre Arduino, vous devrez peut-être utiliser une bibliothèque différente ou modifier le code pour qu'il fonctionne correctement.

## Voir aussi

Pour de plus amples informations, suivez ces liens:
- [HTTP Client Library](https://arduino-esp8266.readthedocs.io/en/latest/esp8266httpclient.html)
- [ESP8266WiFi library](https://arduino-esp8266.readthedocs.io/en/latest/esp8266wifi/readme.html)
- [Arduino - Client](https://www.arduino.cc/en/Reference/ClientConstructor)