---
title:                "Envoi d'une requête http avec une authentification de base"
html_title:           "Arduino: Envoi d'une requête http avec une authentification de base"
simple_title:         "Envoi d'une requête http avec une authentification de base"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi ?
En programmation, l'envoi d'une requête HTTP avec une authentification basique est une méthode pour envoyer des informations à un serveur en utilisant un nom d'utilisateur et un mot de passe. Les programmeurs utilisent cette méthode pour communiquer avec des API externes ou pour accéder à des ressources protégées sur un serveur.

## Comment faire :
Voici un exemple de code pour envoyer une requête avec une authentification basique en utilisant Arduino. Les informations d'identification sont stockées dans des variables et le code utilise la bibliothèque ESP8266WiFi.h pour se connecter à internet.

```
// Inclure la bibliothèque nécessaire
#include <ESP8266WiFi.h>

// Définir les informations d'identification
const char* ssid = "MonSSID";
const char* password = "MonMotDePasse";
const char* server = "www.example.com";
const int port = 80;
const char* username = "MonNomUtilisateur";
const char* userpass = "MonMotDePasseUtilisateur";

// Mettre en place la connexion WiFi
WiFiClient client;
void setup() {
  WiFi.begin(ssid, password);
  // Vérifier la connexion
  while (WiFi.status() != WL_CONNECTED) {
    delay(500);
  }
}

// Effectuer la requête HTTP
void loop() {
  // Se connecter au serveur
  if (client.connect(server, port)) {
    // Envoyer les informations d'identification
    client.print(String("GET /action HTTP/1.1\r\n") +
                 "Host: " + server + "\r\n" +
                 "Authorization: Basic " + String(username) + ":" + String(userpass) + "\r\n" +
                 "Connection: close\r\n\r\n");
    // Lire la réponse du serveur
    while (client.available()) {
      String line = client.readStringUntil('\r');
      Serial.print(line);
    }
    // Fermer la connexion
    client.stop();
  }
  // Attendre 10 secondes avant de renvoyer la requête
  delay(10000);
}
```

### Résultat attendu :
Si tout se passe bien, le code affichera la réponse du serveur dans la console de série. Assurez-vous que les informations d'identification soient correctes pour recevoir une réponse valide.

## Plongée en profondeur :
L'authentification basique a été introduite en 1999 dans la spécification HTTP dans le but de fournir un moyen simple pour authentifier les utilisateurs. Cependant, cette méthode n'est pas très sécurisée car les informations d'identification sont transmises en clair sur le réseau. Il existe des alternatives plus sécurisées, comme l'authentification par clé API ou OAuth, qui peuvent être utilisées en fonction de la situation.

L'implémentation de cette méthode en utilisant Arduino peut varier en fonction de la bibliothèque utilisée et du type de connexion WiFi. Il est important de consulter la documentation de la bibliothèque et de s'assurer que le code est adapté à votre situation.

## À voir également :
- [Tutoriel d'Arduino sur l'utilisation de la bibliothèque ESP8266WiFi pour se connecter à internet](https://www.arduino.cc/en/Tutorial/WiFiWebClient)
- [Spécification HTTP - Section d'authentification basique](https://datatracker.ietf.org/doc/html/rfc7617)
- [Article sur les différentes méthodes d'authentification pour les API](https://blog.restcase.com/http-basic-authentication-explained/)
- [Site officiel de la bibliothèque ESP8266WiFi](https://github.com/esp8266/Arduino/tree/master/libraries/ESP8266WiFi)