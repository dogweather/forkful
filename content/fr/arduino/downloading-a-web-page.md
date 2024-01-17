---
title:                "Téléchargement d'une page web"
html_title:           "Arduino: Téléchargement d'une page web"
simple_title:         "Téléchargement d'une page web"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi?

Télécharger une page web est l'action de récupérer les données d'une page Internet et de les afficher sur un écran. Les programmeurs font cela pour avoir accès à des informations précises en ligne, comme les prévisions météorologiques ou les dernières nouvelles.

## Comment faire:

```Arduino
#include <WiFi.h>
#include <HTTPClient.h>

void setup() {
  Serial.begin(115200);

  WiFi.begin("nom_du_réseau", "mot_de_passe"); // se connecter au WiFi
  while (WiFi.status() != WL_CONNECTED) {
    delay(1000);
    Serial.println("Connexion en cours...");
  }

  Serial.println("Connecté au WiFi!");

  HTTPClient http; // créer un objet HTTPClient
  http.begin("https://www.exemple.com/page"); // spécifier l'URL à télécharger

  int codeHTTP = http.GET(); // envoyer une demande GET et stocker le code de retour

  if (codeHTTP > 0) { // vérifier si la demande a été réussie
    String page = http.getString(); // obtenir la page sous forme de chaîne de caractères
    Serial.println(page); // afficher la page sur le moniteur série
  }

  http.end(); // libérer la mémoire
}

void loop() {
  // rien d'autre à faire ici
}
```

Le résultat sera la page téléchargée affichée sur le moniteur série.

## Plongée en profondeur:

Télécharger des pages web est une fonctionnalité courante dans les projets IoT (Internet des Objets). Les alternatives à l'utilisation d'une bibliothèque WiFi et HTTPClient incluent l'utilisation de protocoles de communication tels que MQTT ou CoAP. Dans l'exemple ci-dessus, nous utilisons une connexion sécurisée (https) pour télécharger la page, mais cela peut aussi être fait en utilisant une connexion non sécurisée (http).

## À voir aussi:

- [WiFi Library Reference](https://www.arduino.cc/en/Reference/WiFi)
- [HTTPClient Library Reference](https://www.arduino.cc/en/Reference/HTTPClient)