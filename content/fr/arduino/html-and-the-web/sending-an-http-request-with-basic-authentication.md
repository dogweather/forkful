---
date: 2024-01-20 18:00:46.377056-07:00
description: "Comment faire: Historiquement, l'authentification de base HTTP a \xE9\
  t\xE9 l'une des premi\xE8res m\xE9thodes pour s\xE9curiser l'acc\xE8s aux ressources\
  \ web. Elle reste\u2026"
lastmod: '2024-04-05T21:53:59.544713-06:00'
model: gpt-4-1106-preview
summary: "Historiquement, l'authentification de base HTTP a \xE9t\xE9 l'une des premi\xE8\
  res m\xE9thodes pour s\xE9curiser l'acc\xE8s aux ressources web."
title: "Envoi d'une requ\xEAte HTTP avec authentification de base"
weight: 45
---

## Comment faire:
```Arduino
#include <ESP8266WiFi.h>
#include <Base64.h>

const char* ssid = "VOTRE_SSID"; // Remplacez avec votre SSID
const char* password = "VOTRE_MOT_DE_PASSE"; // Remplacez avec votre mot de passe
const char* server = "votre.serveur.com";
const char* user = "votre_utilisateur";
const char* pass = "votre_mot_de_passe";

WiFiClient client;

void setup() {
  Serial.begin(115200);
  WiFi.begin(ssid, password);
  Serial.println("Connexion");

  while (WiFi.status() != WL_CONNECTED) {
    delay(500);
    Serial.print(".");
  }

  if (client.connect(server, 80)) {
    String authValue = "Basic " + base64::encode(String(user) + ":" + String(pass));
    client.println("GET /chemin/ressource HTTP/1.1");
    client.println("Host: " + String(server));
    client.println("Authorization: " + authValue);
    client.println("Connection: close");
    client.println();
  }
}

void loop() {
  delay(10000); // Un délai pour de futures requêtes
}

```

## Exploration
Historiquement, l'authentification de base HTTP a été l'une des premières méthodes pour sécuriser l'accès aux ressources web. Elle reste une solution simple bien qu'elle ne soit pas la plus sûre. Alternativement, on utilise souvent l'authentification par jeton (token) comme OAuth. Concernant Arduino, l'utilisation de bibliothèques comme ESP8266WiFi facilite l'envoi des requêtes HTTP. Attention, l'authentification de base doit être utilisée avec HTTPS pour éviter l'exposition des credentials en clair.

## Voir aussi
- Documentation Arduino ESP8266WiFi: https://arduino-esp8266.readthedocs.io/en/latest/esp8266wifi/readme.html
- Guide de l'authentification HTTP de base: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication
- Base64 encoding with Arduino: https://www.arduino.cc/reference/en/libraries/base64/

(Remarquez que les liens sont en anglais, car les ressources en français sont moins fréquentes pour des sujets techniques spécifiques comme celui-ci.)
