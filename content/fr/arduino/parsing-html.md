---
title:                "Analyse syntaxique de HTML"
html_title:           "Bash: Analyse syntaxique de HTML"
simple_title:         "Analyse syntaxique de HTML"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/parsing-html.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

Le parsing HTML, ou l'analyse syntaxique HTML, c'est le processus d'extraction de données spécifiques d'une page Web. Les programmeurs le font pour automatiser la collecte d'informations à partir du Web, économisant ainsi du temps et des efforts.

## Comment faire :

Nous allons utiliser une bibliothèque appelée Arduino Http Client pour obtenir les données d'une page HTML. Voici un exemple de code pour illustrer le parsing HTML avec Arduino.

```Arduino
#include <ArduinoHttpClient.h>

char server[] = "www.votre-serveur.fr";
int port = 80;

EthernetClient client;
HttpClient http = HttpClient(client, server, port);
int status = http.get("/");

if (status == 0) {
  while (http.connected()) {
    String line = http.readStringUntil('\n');
    Serial.println(line);
  }
} 
else {
  Serial.println("Erreur lors de la connexion");
}
```

Lorsque vous exécutez ce code, vous verrez la sortie HTML de la page que vous avez demandée.

## Plongée en profondeur

Historiquement, le parsing HTML était réalisé à l'aide de regex (expressions régulières), mais cette méthode a souvent prouvé ses limites. Aujourd'hui, des bibliothèques spéciales comme Arduino HttpClient sont utilisées à la place. L'alternative moderne à Arduino HttpClient est la bibliothèque ESP (pour des chipsets comme ESP32 ou ESP8266) qui fournit également une interface de parsing HTML. La mise en œuvre de l'analyse HTML avec ces bibliothèques repose sur le concept de Finite State Machine (FSM) ou Automate à États Finis en français.

## Voir aussi

Pour plus d'informations, consultez ces ressources:

1. http://arduino.esp8266.com/ : Documentation sur la bibliothèque ESP et ses fonctionnalités relatives à l'analyse HTML.
2. https://www.arduino.cc/en/Tutorial/HomePage : Tutoriels Arduino officiels qui détaillent divers aspects du codage avec Arduino.
3. https://stackoverflow.com/questions/tagged/arduino?tab=Votes : Questions et réponses courantes sur l'Arduino, y compris la manipulation de HTML.