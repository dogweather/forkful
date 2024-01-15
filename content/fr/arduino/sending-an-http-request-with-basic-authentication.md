---
title:                "Envoyer une demande http avec une authentification de base"
html_title:           "Arduino: Envoyer une demande http avec une authentification de base"
simple_title:         "Envoyer une demande http avec une authentification de base"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous vous demandez peut-être pourquoi quelqu'un voudrait envoyer une requête HTTP avec une authentification de base lors d'une programmation Arduino. Eh bien, cela peut être utile lorsque vous devez accéder à des données sensibles sur un serveur distant, telles qu'une API de données météorologiques ou une base de données IoT.

## Comment faire

La première étape pour envoyer une requête HTTP avec une authentification de base est de déterminer l'URL de destination et les informations d'identification nécessaires. Ensuite, vous devrez inclure la bibliothèque WiFiClientSecure et créer un nouvel objet WiFiClientSecure. Enfin, utilisez la méthode connect() pour établir une connexion sécurisée au serveur et utilisez la méthode print() pour envoyer votre requête.

Voici un exemple de code pour une requête HTTP GET avec une authentification de base:

```
#include <WiFiClientSecure.h>

const char* ssid = "MON_RESEAU_WIFI";
const char* password = "MON_MOT_DE_PASSE_WIFI";

const char* url = "MON_URL";
const char* auth = "MON_NOM_D'UTILISATEUR:MON_MOT_DE_PASSE";
// Remplacez les informations ci-dessus par les vôtres

WiFiClientSecure client;

void setup() {
  Serial.begin(115200);
  WiFi.begin(ssid, password);
  Serial.print("Connexion au WiFi...");
  while (WiFi.status() != WL_CONNECTED) {
    Serial.print(".");
    delay(500);
  }
  Serial.println();
  Serial.println("Connecté au WiFi!");
}

void loop() {
  if (client.connect(url, 443)) {
    Serial.println("Connexion établie!");
    String request = "GET / HTTP/1.1\r\nAuthorization: Basic ";
    request += base64::encode(auth) + "\r\n\r\n";
    client.print(request);
    Serial.print("Requête envoyée: ");
    Serial.println(request);
  }
  else {
    Serial.println("La connexion a échoué.");
  }
}
```

Lors de l'exécution de ce code, vous devriez voir dans la sortie série que la connexion est établie et que la requête est envoyée avec succès.

## Deep Dive

Envoyer une requête HTTP avec une authentification de base implique d'encoder les informations d'identification en utilisant l'algorithme Base64. Cela convertit les caractères ASCII en une chaîne de texte codée, qui empêche les utilisateurs malveillants de lire facilement les informations d'identification.

De plus, vous pouvez également spécifier une méthode autre que GET dans votre requête, telle que POST ou PUT, en incluant la ligne "Content-Type: application/x-www-form-urlencoded" dans votre requête et en ajoutant les données souhaitées après la ligne vide dans la requête. Cela peut être utile si vous souhaitez envoyer des données à un serveur pour les traiter.

## Voir aussi

Pour en savoir plus sur les requêtes HTTP avec Arduino, vous pouvez consulter les liens suivants:

- [Documentation officielle des bibliothèques WiFiClientSecure et WiFi](https://www.arduino.cc/en/Reference/WiFiClientSecure)
- [Tutoriel vidéo sur l'utilisation de la bibliothèque WiFiClientSecure](https://www.youtube.com/watch?v=dO2ic6oBlhU)
- [Guide étape par étape pour envoyer des données à un serveur avec Arduino](https://randomnerdtutorials.com/esp8266-http-get-post-requests/)