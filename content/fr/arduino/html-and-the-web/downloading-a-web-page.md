---
date: 2024-01-20 17:43:29.348322-07:00
description: "Comment faire : Historiquement, les Arduinos \xE9taient utilis\xE9s\
  \ pour des projets locaux. Avec l\u2019av\xE8nement d'Arduino \xE9quip\xE9s de WiFi\
  \ comme l'ESP8266 ou\u2026"
lastmod: '2024-04-05T22:51:12.023710-06:00'
model: gpt-4-1106-preview
summary: "Historiquement, les Arduinos \xE9taient utilis\xE9s pour des projets locaux."
title: "T\xE9l\xE9chargement d'une page web"
weight: 42
---

## Comment faire :
```Arduino
#include <ESP8266WiFi.h>

const char* ssid = "Votre_SSID";
const char* password = "Votre_mot_de_passe";
const char* host = "example.com";

void setup() {
  Serial.begin(115200);
  delay(10);

  // Connectez-vous au Wi-Fi
  Serial.println();
  Serial.println();
  Serial.print("Connecting to ");
  Serial.println(ssid);

  WiFi.begin(ssid, password);

  while (WiFi.status() != WL_CONNECTED) {
    delay(500);
    Serial.print(".");
  }

  Serial.println("");
  Serial.println("WiFi connected");

  // Debut de la connexion avec le serveur web
  WiFiClient client;
  const int httpPort = 80;
  if (!client.connect(host, httpPort)) {
    Serial.println("Connection failed");
    return;
  }

  // Envoie d'une requete HTTP
  client.print(String("GET /") + " HTTP/1.1\r\n" +
               "Host: " + host + "\r\n" + 
               "Connection: close\r\n\r\n");

  // Attente de la reponse du serveur
  while(client.available() == 0) {
    if (!client.connected()) {
      Serial.println("Server disconnected!");
      client.stop();
      return;
    }
  }

  // Lecture de la premiere ligne du serveur Web
  String line = client.readStringUntil('\r');
  Serial.print("Status Line: ");
  Serial.println(line);
}

void loop() {
  // execution recurrente non nécessaire ici
}
```

Sortie échantillon :
```
Connecting to Votre_SSID
......
WiFi connected
Status Line: HTTP/1.1 200 OK
```

## Exploration approfondie :
Historiquement, les Arduinos étaient utilisés pour des projets locaux. Avec l’avènement d'Arduino équipés de WiFi comme l'ESP8266 ou l'ESP32, il est possible de télécharger des pages web, ouvrant ainsi la voie à l’Internet des Objets (IoT). Alternatives ? Il existe d'autres manières d'interagir avec le web, comme l'utilisation d'API REST, MQTT pour la communication entre appareils IoT, ou même d'autres cartes comme Raspberry Pi pour des tâches plus lourdes. Les détails criticels de la mise en œuvre incluent la sécurisation des connexions via HTTPS, la gestion des requêtes asynchrones pour ne pas bloquer le programme, et le parsing efficace des données reçues.

## À voir également :
- Documentation Arduino sur la gestion du WiFi : https://www.arduino.cc/en/Reference/WiFi
- Guide d'utilisation de l'ESP8266 avec Arduino : https://randomnerdtutorials.com/projects-esp8266/
- Tutoriels sur l'interaction avec des APIs web via Arduino : https://www.arduino.cc/en/Tutorial/WebClient  
- Introduction à l'IoT et Arduino : https://www.arduino.cc/en/IoT/HomePage
