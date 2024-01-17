---
title:                "Interprétation HTML"
html_title:           "Arduino: Interprétation HTML"
simple_title:         "Interprétation HTML"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/parsing-html.md"
---

{{< edit_this_page >}}

## Qu'est-ce que le parsing HTML et pourquoi les programmeurs le font-ils?

Le parsing HTML est le processus de séparation d'un document HTML en différents éléments ou balises pour en extraire les informations utiles. Les programmeurs le font pour pouvoir traiter et manipuler ces informations dans leur code.

## Comment faire :

```
Arduino code :
#include <ESP8266HTTPClient.h>
#include <ESP8266WiFi.h>

WiFiClient client;
HTTPClient http;

// Connexion à un réseau WiFi
WiFi.begin("SSID", "Mot de passe");

// Vérifie si la connexion est établie
while (WiFi.status() != WL_CONNECTED) {
  delay(500);
}

// Se connecte à une URL et récupère le contenu de la page
http.begin(client, "www.example.com");
int httpResponseCode = http.GET();
String response = http.getString();

// Trouve et stocke les informations de tags HTML spécifiques
int start = response.indexOf("<h1>") + 4;
int end = response.indexOf("</h1>");
String title = response.substring(start, end);

// Affiche le titre récupéré
Serial.println(title);
```

## Deep Dive :

Le parsing HTML est une technique courante utilisée dans la programmation, en particulier dans le domaine du web, où les données sont souvent stockées dans des documents HTML. Il existe plusieurs alternatives pour réaliser cette opération, telles que l'utilisation d'expressions régulières ou de bibliothèques spécialisées, mais l'utilisation de fonctions de manipulation de chaînes de caractères et la recherche de balises HTML spécifiques reste l'approche la plus simple et efficace pour Arduino.

## Voir aussi :

- [Tutorialspoint - HTML Parsing](https://www.tutorialspoint.com/html-parsing)
- [HTML Dog - Introduction to the DOM](https://www.htmldog.com/guides/javascript/advanced/intro/)
- [W3Schools - String Object Methods](https://www.w3schools.com/js/js_string_methods.asp)