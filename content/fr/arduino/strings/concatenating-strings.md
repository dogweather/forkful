---
aliases:
- /fr/arduino/concatenating-strings/
date: 2024-01-20 17:33:51.943093-07:00
description: "Concat\xE9ner des cha\xEEnes, c'est les assembler bout \xE0 bout. On\
  \ le fait pour construire du texte dynamiquement, combiner des donn\xE9es ou cr\xE9\
  er des commandes."
lastmod: 2024-02-18 23:09:09.106298
model: gpt-4-1106-preview
summary: "Concat\xE9ner des cha\xEEnes, c'est les assembler bout \xE0 bout. On le\
  \ fait pour construire du texte dynamiquement, combiner des donn\xE9es ou cr\xE9\
  er des commandes."
title: "Concat\xE9nation de cha\xEEnes de caract\xE8res"
---

{{< edit_this_page >}}

## What & Why?
Concaténer des chaînes, c'est les assembler bout à bout. On le fait pour construire du texte dynamiquement, combiner des données ou créer des commandes.

## How to:
```Arduino
void setup() {
  Serial.begin(9600);
  String salut = "Bonjour";
  String nom = "Monde";
  String phrase = salut + ", " + nom + "!";
  Serial.println(phrase); // Affiche: Bonjour, Monde!
}

void loop() {
  // Rien ici pour l'instant.
}
```

## Deep Dive
Concaténer des chaînes est essentiel depuis les débuts de la programmation. En C, on utilisait `strcat()` mais Arduino propose l'objet `String`, plus simple. Attention, abuser des `String` peut fragmenter la mémoire sur des systèmes avec peu de ressources. Une alternative : utiliser `snprintf()`, plus compliquée mais plus efficace en mémoire.

## See Also
- [Arduino Reference: String Object](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- [Arduino Forum: String concatenation](https://forum.arduino.cc/index.php?topic=396450)
- [Arduino Memory Optimization](https://www.arduino.cc/en/Tutorial/Memory)
