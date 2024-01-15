---
title:                "Télécharger une page web."
html_title:           "Arduino: Télécharger une page web."
simple_title:         "Télécharger une page web."
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Pourquoi

Vous êtes peut-être curieux de savoir comment télécharger une page Web en utilisant votre carte Arduino, ou peut-être avez-vous un projet qui nécessite cette fonctionnalité. Dans cet article, nous allons vous montrer comment télécharger une page Web avec votre carte Arduino en utilisant le langage de programmation Arduino.

# Comment faire

Pour télécharger une page Web avec votre carte Arduino, vous devez suivre ces étapes simples :

- Ouvrez votre IDE Arduino et créez un nouveau sketch.
- Copiez et collez le code suivant dans votre esquisse :

```Arduino
#include <SPI.h>
#include <WiFiNINA.h>

char ssid[] = "VotreSSID";
char pass[] = "VotreMotDePasse";

int status = WL_IDLE_STATUS;
WiFiClient client;

void setup() {
  // Initialise la communication série
  Serial.begin(9600);

  // Se connecte à votre réseau WiFi
  status = WiFi.begin(ssid, pass);

  // Vérifie si la connexion a été établie avec succès
  if (status != WL_CONNECTED) {
    Serial.println("Impossible de se connecter au réseau WiFi !");
    while(true);
  }
}

void loop() {
  // Se connecte à l'adresse IP de la page Web
  if (client.connect("www.example.com", 80)) {
    // Envoi de la requête GET pour télécharger la page
    client.println("GET / HTTP/1.1");
    client.println("Host: www.example.com");
    client.println("Connection: close");
    client.println();
  }

  // Affiche la réponse de la page Web sur le moniteur série
  while (client.available()) {
    char c = client.read();
    Serial.write(c);
  }

  // Déconnecte le client
  client.stop();

  // Attendre 30 secondes avant de télécharger à nouveau
  delay(30000);
}
```

- Assurez-vous de remplacer "VotreSSID" et "VotreMotDePasse" par les informations de votre propre réseau WiFi.
- Téléversez le sketch sur votre carte Arduino et ouvrez le moniteur série pour voir la réponse de la page Web.

Le moniteur série devrait afficher le contenu HTML de la page Web que vous avez téléchargée. Vous pouvez également modifier le code pour télécharger différentes pages Web en changeant l'URL et la méthode HTTP (GET, POST, etc.).

# Plongée en profondeur

Il existe de nombreuses façons de télécharger une page Web avec votre carte Arduino, en utilisant différentes bibliothèques et méthodes. Dans cet exemple, nous avons utilisé la bibliothèque WiFiNINA pour gérer la connexion WiFi et la classe WiFiClient pour communiquer avec le serveur.

Il est important de noter que cette méthode de téléchargement de page Web n'est pas adaptée pour les gros fichiers en raison des limitations de mémoire de la carte Arduino. Si vous avez besoin de télécharger des fichiers plus volumineux, vous devriez envisager d'utiliser une carte avec un processeur plus puissant ou de trouver une autre méthode de téléchargement.

# Voir aussi

- [Documentation officielle de WiFiNINA](https://www.arduino.cc/en/Reference/WiFiNINA)
- [Tutoriel sur l'utilisation du module WiFi NINA pour l'ESP32](https://www.arduino.cc/en/Guide/NINAWiFiESP32)
- [Bibliothèque HTTPClient pour l'Arduino](https://github.com/arduino-libraries/ArduinoHttpClient)