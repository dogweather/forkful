---
title:                "Télécharger une page web"
html_title:           "Bash: Télécharger une page web"
simple_title:         "Télécharger une page web"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Qu'est-ce & Pourquoi?

Télécharger une page web consiste à récupérer son contenu et à le stocker pour une lecture hors ligne. Les programmeurs le font pour l'analyse de données, le web scraping, la sauvegarde de l'information et le test de la connectivité.

## Comment faire :

Voici un exemple simple de comment télécharger une page Web en utilisant Arduino :

```Arduino
#include <SPI.h>
#include <Ethernet.h>

byte mac[] = {  
  0xDE, 0xAD, 0xBE, 0xEF, 0xFE, 0xED };

char server[] = "www.example.com";

EthernetClient client;

void setup() {
  Serial.begin(9600);

  if (Ethernet.begin(mac) == 0) {
    Serial.println("Failed to configure Ethernet");
    return;
  }

  delay(1000);

  if (client.connect(server, 80)) {
    client.println("GET / HTTP/1.1");
    client.println("Host: www.example.com");
    client.println("Connection: close");
    client.println();
  }
  else {
    Serial.println("Connection failed");
  }
}

void loop() {
  if (client.available()) {
    char c = client.read();
    Serial.print(c);
   }

  if (!client.connected()) {
    Serial.println();
    Serial.println("Disconnecting.");
    client.stop();
    for(;;)
      ;
  }
}
```

Ce programme connecte Arduino à Internet via ethernet, se connecte à un serveur web et télécharge une page. 

## Approfondissement

Le téléchargement d'une page web avec Arduino est un concept relativement moderne qui a émergé avec l'avènement des cartes de communication réseau pour les microcontrôleurs. Des alternatives comme les modules wifi ESP8266 ou ESP32 peuvent être utilisées pour réaliser une telle opération d'une manière plus sophistiquée. Les détails d'implémentation pour le téléchargement de la page web impliquent établir une connexion TCP avec le serveur, envoyer une requête HTTP GET et lire la réponse.

## Voir aussi :

- Guide approfondi sur le protocole HTTP : http://www.ntu.edu.sg/home/ehchua/programming/webprogramming/HTTP_Basics.html 
- Utilisation de la bibliothèque Ethernet Arduino : https://www.arduino.cc/en/Reference/Ethernet 
- Guide pour télécharger une page web en Python pour la comparaison : https://www.geeksforgeeks.org/python-how-to-request-webpages/.
- Utilisation des modules ESP8266 et ESP32 : https://randomnerdtutorials.com/