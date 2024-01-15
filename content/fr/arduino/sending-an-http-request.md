---
title:                "Envoyer une requête http"
html_title:           "Arduino: Envoyer une requête http"
simple_title:         "Envoyer une requête http"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Pourquoi

## Pourquoi utiliser une requête HTTP sur Arduino ?

Si vous voulez que votre projet Arduino soit connecté à Internet, vous aurez besoin d'envoyer des requêtes HTTP. Cela peut vous permettre d'obtenir des informations à partir d'un serveur ou de contrôler des dispositifs à distance.

# Comment faire

## Comment envoyer une requête HTTP avec Arduino ?

Pour envoyer une requête HTTP avec Arduino, vous aurez besoin d'une connexion Internet et d'un bouclier Ethernet ou WiFi. Voici un exemple de code pour envoyer une requête GET à un serveur et afficher la réponse dans le moniteur série :

```Arduino 
#include <SPI.h> // Inclure la bibliothèque SPI
#include <Ethernet.h> // Inclure la bibliothèque Ethernet

byte mac[] = { 0xDE, 0xAD, 0xBE, 0xEF, 0xFE, 0xED }; // Définir l'adresse MAC
IPAddress server(192,168,1,1); // Définir l'adresse IP du serveur
EthernetClient client; // Créer un objet client

void setup() {
  Serial.begin(9600); // Initialiser le moniteur série
  Ethernet.begin(mac); // Initialiser la connexion Ethernet
  delay(1000); // Attendre 1 seconde
}

void loop() {
  if (client.connect(server, 80)) { // Si la connexion au serveur réussit
    Serial.println("Connexion établie."); // Afficher un message
    client.println("GET / HTTP/1.1"); // Envoyer la requête GET
    client.println("Host: 192.168.1.1"); // Ajouter l'en-tête Host
    client.println("Connection: close"); // Ajouter l'en-tête Connection pour fermer la connexion après la réponse
    client.println(); // Terminer la requête avec une ligne vide
  } else {
    Serial.println("Échec de la connexion."); // Afficher un message en cas d'échec
  }
 
  while (client.available()) { // Tant qu'il y a des données à lire
    char c = client.read(); // Lire un caractère
    Serial.print(c); // Afficher le caractère dans le moniteur série
  }

  if (!client.connected()) { // Si la connexion est fermée
    client.stop(); // Fermer la connexion
    Serial.println("\nConnexion terminée."); // Afficher un message
    while(true); // Attendre en boucle jusqu'à la prochaine connexion
  }
}
```

Lorsque vous téléverserez ce code sur votre carte Arduino, vous devriez voir la réponse du serveur dans le moniteur série.

# Deep Dive

## En savoir plus sur l'envoi de requêtes HTTP avec Arduino

Voici quelques éléments à prendre en compte lorsque vous utilisez des requêtes HTTP sur Arduino :

- Vous pouvez également envoyer des requêtes POST pour envoyer des données au serveur.
- Vous pouvez ajouter des en-têtes supplémentaires à la requête en utilisant la fonction `client.println()`, comme dans l'exemple ci-dessus.
- Vous pouvez utiliser un shield ou un module WiFi pour connecter votre Arduino à Internet.
- Certaines bibliothèques peuvent faciliter l'envoi de requêtes HTTP sur Arduino, comme `HTTPClient.h` ou `WiFiClientSecure.h` pour utiliser une connexion sécurisée.

En utilisant les requêtes HTTP, vous pouvez accéder à une multitude de services en ligne et contrôler votre Arduino à distance. Cela peut être très utile pour les projets de domotique, de surveillance ou de suivi de données en temps réel. N'hésitez pas à explorer davantage cette fonctionnalité et à l'implémenter dans vos projets !

# Voir aussi

- [Tutoriel sur l'utilisation de requêtes HTTP avec Arduino](https://www.arduino.cc/en/Tutorial/LibraryExamples/HTTPClient)
- [Documentation Ethernet.h](https://www.arduino.cc/en/Reference/Ethernet)
- [Documentation WiFiClient.h](https://www.arduino.cc/en/Reference/WiFiClient)
- [Autre exemple de requête HTTP avec WiFiClientSecure.h](https://randomnerdtutorials.com/esp32-http-get-parameters-arduino-ide/)