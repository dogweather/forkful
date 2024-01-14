---
title:                "Arduino: Télécharger une page web"
simple_title:         "Télécharger une page web"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous êtes peut-être débutant en programmation Arduino et vous vous demandez pourquoi télécharger une page web peut être utile. Eh bien, cela vous permettra de récupérer facilement des données en temps réel à partir d'Internet et de les utiliser dans vos projets.

## Comment faire

Télécharger une page web en utilisant Arduino peut sembler compliqué, mais c'est en fait assez simple avec la bibliothèque "Ethernet.h" intégrée. Voici un exemple de code pour vous aider à démarrer :

```Arduino
#include <SPI.h>
#include <Ethernet.h>

byte mac[] = { 0xDE, 0xAD, 0xBE, 0xEF, 0xFE, 0xED }; //définir l'adresse MAC de votre carte Ethernet
IPAddress ip(192, 168, 1, 10); //définir l'adresse IP de votre carte Ethernet
char server[] = "www.example.com";  //adresse du serveur où se trouve la page web

EthernetClient client; //créer un objet client pour établir la connexion

void setup() {
  Ethernet.begin(mac, ip);  //démarrer la connexion Ethernet
  Serial.begin(9600);  //démarrer la communication série pour afficher les données récupérées
  delay(1000); //attente pour s'assurer que la connexion est établie
}

void loop() {
  if (client.connect(server, 80)) {//si une connexion peut être établie
    Serial.println("Connexion établie");
    client.println("GET /index.html HTTP/1.1"); //demander la page web souhaitée
    client.println("Host: www.example.com"); //spécifier le serveur
    client.println("Connection: close"); //fermer la connexion après la demande
    client.println(); //fin de la demande
  } else {
    Serial.println("Connexion échouée");
  }
  while (client.available()) { //tant que des données sont disponibles
    char c = client.read(); //lire les données reçues
    Serial.print(c); //afficher les données sur la console série
  }
  delay(10000); //attente pour répéter la demande toutes les 10 secondes
}
```

Lorsque vous téléversez ce code sur votre carte Arduino, assurez-vous d'avoir une connexion Ethernet établie et de changer les adresses MAC, IP et serveur selon vos besoins. Vous verrez alors les données de la page web s'afficher sur la console série.

## Plongée profonde

Maintenant que vous savez comment télécharger une page web avec Arduino, il est important de noter que cette méthode utilise une connexion HTTP non sécurisée. Si vous souhaitez récupérer des données à partir d'une page web sécurisée en utilisant HTTPS, vous devrez utiliser une bibliothèque tierce telle que "WiFiClientSecure.h".

Cette bibliothèque permet une communication HTTPS sécurisée en utilisant des certificats pour vérifier l'authenticité du serveur. Cependant, cela nécessite une carte WiFi compatible avec la bibliothèque et une connexion WiFi établie avant de pouvoir télécharger la page web.

## Voir aussi

- Tutoriel officiel de téléchargement d'une page web avec Arduino : https://www.arduino.cc/en/Tutorial/WebClient
- Bibliothèque Ethernet.h : https://www.arduino.cc/en/Reference/Ethernet
- Bibliothèque WiFiClientSecure.h : https://github.com/esp8266/Arduino/tree/master/libraries/ESP8266WiFi/src
- Tutoriel pour configurer une connexion Ethernet avec un Arduino : https://www.arduino.cc/en/Guide/Ethernet
- Tutoriel pour configurer une connexion WiFi avec un Arduino : https://www.arduino.cc/en/Guide/ArduinoWiFi