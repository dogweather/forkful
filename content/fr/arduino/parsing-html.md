---
title:                "Analyse des pages HTML"
html_title:           "Arduino: Analyse des pages HTML"
simple_title:         "Analyse des pages HTML"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/parsing-html.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous voulez utiliser des données provenant de sites web dans votre projet Arduino, il peut être utile de savoir comment analyser le code HTML. Cela vous permettra de récupérer facilement les informations dont vous avez besoin et de les utiliser dans votre code.

## Comment faire

Pour analyser le code HTML en utilisant Arduino, vous pouvez utiliser une bibliothèque appelée "HtmlParser". Voici un exemple de code pour extraire le titre d'une page web :

```Arduino
#include <HtmlParser.h>

HtmlParser html;

void setup(){
  Serial.begin(9600);
  html.parseUrl("https://www.example.com"); //remplacez l'url par celle que vous voulez analyser
}

void loop(){
  String title = html.getTagValue("title"); //récupère la valeur de la balise "title"
  Serial.println(title); //affiche le titre dans le moniteur série
  delay(5000); //attend 5 secondes avant de refaire la boucle
}
``` 

Output : "Example Website"

## Plongée en profondeur

La bibliothèque HtmlParser offre plusieurs fonctions utiles pour analyser le code HTML. Voici quelques-unes d'entre elles :

- `parseUrl(url)` : permet de spécifier l'URL à analyser
- `getTagValue(tag)` : extrait la valeur de la balise spécifiée
- `getFirstTagName()` : renvoie le nom de la première balise trouvée dans la page
- `getNextTag()` : permet de passer à la balise suivante
- `reset()` : remet à zéro l'analyse, utile si vous voulez analyser une nouvelle page avec la même instance de la bibliothèque.

Il est également possible d'analyser des balises spécifiques à l'aide de l'index ou du nom de la balise :

```Arduino
#include <HtmlParser.h>

HtmlParser html;

void setup(){
  Serial.begin(9600);
  html.parseUrl("https://www.example.com");
}

void loop(){
  html.reset();
  while(html.getNextTag() != NULL){ //tant qu'il y a encore des balises à analyser
    if(html.getTagName() == "p"){ //si la balise est une balise "p"
      String content = html.getTagValue(); //récupère le contenu de la balise (texte entre les balises ouvrantes et fermantes)
      Serial.println(content); //affiche le contenu dans le moniteur série
    }
  }
  delay(5000);
}
```

Output :

"Premier paragraphe du site Example"

"Deuxième paragraphe du site Example"

## Voir aussi

- Documentation officielle de la bibliothèque HtmlParser : https://github.com/zhijunzhou/Arduino-HTML-Parser
- Tutoriel sur l'utilisation de la bibliothèque HtmlParser : https://learn.adafruit.com/html-coding-for-html-parsing-with-arduino/conclusion