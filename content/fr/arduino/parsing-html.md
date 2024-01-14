---
title:                "Arduino: Analyse du html"
simple_title:         "Analyse du html"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/parsing-html.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous voulez créer un projet Arduino qui nécessite de récupérer des données à partir d'un site web ? Alors savoir comment analyser le code HTML peut être très utile pour extraire les informations dont vous avez besoin. Dans cet article, nous allons vous montrer comment le faire en utilisant Arduino.

## Comment faire

Pour commencer, il faut télécharger la bibliothèque "HTTPClient" sur l'IDE Arduino. Ensuite, vous pouvez utiliser la fonction "GET" pour récupérer la page web de votre choix. Voici un exemple de code pour récupérer le titre d'une page web :

```Arduino
#include <HTTPClient.h>

String url = "www.example.com";
HTTPClient http;

http.begin(url);
int status = http.GET();

if(status > 0){
  String html = http.getString();
  int titleStart = html.indexOf("<title>");
  int titleEnd = html.indexOf("</title>");
  
  String title = html.substring(titleStart+7, titleEnd);
  Serial.println(title);
}
```

Dans cet exemple, nous utilisons la fonction "indexOf" pour trouver la position du début et de la fin de la balise "title" dans le code HTML. Ensuite, nous utilisons la fonction "substring" pour extraire uniquement le titre de la page. Vous pouvez adapter ce code en fonction de vos besoins pour extraire d'autres données.

## Plongée en profondeur

Il est important de noter que cette méthode n'est pas toujours fiable car elle dépend de la structure du code HTML de la page web. Si le code change, votre fonction ne fonctionnera plus correctement. De plus, si le site web utilise des techniques de défense contre le parsing, votre code pourrait être bloqué.

Il existe également d'autres bibliothèques et méthodes pour analyser du code HTML sur Arduino, vous pouvez donc explorer différentes options en fonction de votre projet.

## Voir aussi

- Tutoriel pour analyser du code HTML sur Arduino : [lien vers un tutoriel en ligne]
- Plus de détails sur la bibliothèque "HTTPClient" : [lien vers la documentation]
- Exemples de projets utilisant l'analyse de code HTML sur Arduino : [liens vers des projets en ligne]