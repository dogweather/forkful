---
title:                "Concaténer des chaînes de caractères"
html_title:           "Arduino: Concaténer des chaînes de caractères"
simple_title:         "Concaténer des chaînes de caractères"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

## Quoi et pourquoi?
Quand on programme, on a souvent besoin de combiner plusieurs chaînes de caractères en une seule. Cela s'appelle "concaténer des chaînes" et c'est une pratique courante pour les programmeurs. Cela peut être utile pour créer des messages, des instructions ou des variables dynamiquement.

## Comment faire:
Voici comment concaténer des chaînes dans un code Arduino:

```
//Déclaration de deux chaînes de caractères
String chaine1 = "Bonjour ";
String chaine2 = "le monde!";

//Concaténation des deux chaînes avec un "+" entre elles
String chaineFinale = chaine1 + chaine2;

//Affichage de la chaîne concaténée
Serial.println(chaineFinale);

//Résultat: Bonjour le monde!
```

## Plongée en profondeur:
La concaténation de chaînes existe depuis les débuts de la programmation informatique. Auparavant, elle était réalisée manuellement en réservant un espace mémoire suffisant pour contenir toutes les chaînes à concaténer. Aujourd'hui, de nombreux langages de programmation, dont Arduino, offrent des fonctions simples pour le faire.

Il existe également d'autres méthodes pour combiner des chaînes, telles que l'utilisation de tableaux de caractères, mais la concaténation reste la plus courante.

L'implémentation de la concaténation de chaînes dans Arduino utilise principalement la classe "String" qui offre des fonctions pratiques pour la manipulation de chaînes.

## Voir aussi:
En savoir plus sur la concaténation de chaînes dans Arduino:
- [Documentation officielle](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/)
- [Tutoriel en français](https://www.tutorialspoint.com/arduino/arduino_strings.htm)