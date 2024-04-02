---
date: 2024-01-20 17:35:51.305895-07:00
description: "Convertir une date en cha\xEEne de caract\xE8res consiste \xE0 transformer\
  \ une repr\xE9sentation de date (typiquement num\xE9rique) en texte lisible. Les\
  \ programmeurs\u2026"
lastmod: '2024-03-13T22:44:58.126029-06:00'
model: gpt-4-1106-preview
summary: "Convertir une date en cha\xEEne de caract\xE8res consiste \xE0 transformer\
  \ une repr\xE9sentation de date (typiquement num\xE9rique) en texte lisible. Les\
  \ programmeurs\u2026"
title: "Conversion d'une date en cha\xEEne de caract\xE8res"
weight: 28
---

## What & Why?
Convertir une date en chaîne de caractères consiste à transformer une représentation de date (typiquement numérique) en texte lisible. Les programmeurs font cela pour enregistrer, afficher ou communiquer des dates de façon compréhensible pour les humains.

## How to:
Utiliser `sprintf` pour formater une date en chaîne de caractères (la date ici est le 5 avril 2023) :

```Arduino
char dateString[11]; // En format "JJ/MM/AAAA\0"
int jour = 5;
int mois = 4;
int annee = 2023;

void setup() {
  // Commencer la communication série
  Serial.begin(9600);
  
  // Convertir la date en chaîne
  sprintf(dateString, "%02d/%02d/%04d", jour, mois, annee);
  
  // Afficher la chaîne convertie
  Serial.println(dateString);
}

void loop() {
  // Pas nécessaire ici
}
```

Sortie :
```
05/04/2023
```

## Deep Dive
Historiquement, la représentation des dates était principalement numérique, économisant de l'espace dans les systèmes informatiques. De nos jours, avec une capacité de stockage accrue, l'affichage lisible (string) prend plus d'importance pour l'interaction avec les utilisateurs. La fonction `sprintf` est un outil classique de C++ disponible dans l'environnement Arduino pour la conversion de données en chaînes de caractères. Il existe des alternatives comme l'utilisation de la bibliothèque `Time` pour des fonctionnalités de date et d'heure plus complexes. L'implémentation de `sprintf` en Arduino peut varier, notamment pour les formats longs comme les flottants; pour une date, cependant, elle est stable et efficace.

## See Also
- [Bibliothèque Time pour Arduino](https://playground.arduino.cc/Code/Time/)
- [Guide sur l'utilisation des chaînes de caractères en Arduino](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
