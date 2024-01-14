---
title:                "Arduino: Concaténation de chaînes de caractères"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

## Pourquoi

Concaténer des chaînes de caractères est utile lorsque l'on souhaite créer du texte dynamique dans un programme Arduino. Cela permet de combiner plusieurs chaînes de caractères en une seule, ce qui peut être pratique pour afficher des messages ou des données.

## Comment faire

Pour concaténer des chaînes de caractères en Arduino, il suffit d'utiliser l'opérateur "+" et le symbole "=". Par exemple, si l'on souhaite afficher le message "Bonjour Codeur !" en utilisant deux variables "salutation" et "nom", on peut le faire ainsi :

```Arduino
String salutation = "Bonjour";
String nom = "Codeur !";

String message = salutation + " " + nom; // le symbole "+" concatène les chaînes de caractères
Serial.println(message);

// Output : Bonjour Codeur !
```

## Plongée en profondeur

Il est important de noter que l'utilisation excessive de la concaténation de chaînes de caractères peut entraîner une consommation excessive de mémoire sur votre Arduino. Cela peut être problématique si vous utilisez des chaînes de caractères très longues ou si votre programme doit fonctionner avec une mémoire limitée.

De plus, il est préférable d'utiliser la classe String plutôt que les tableaux de caractères (char) pour les opérations de concaténation. En effet, la classe String gère automatiquement la mémoire et évite les problèmes de dépassement de capacité.

Il est également possible d'utiliser la fonction "concat()" pour concaténer plusieurs chaînes de caractères en une seule. Elle est généralement plus efficace que l'utilisation de l'opérateur "+".

## Voir aussi

- [Documentation officielle d'Arduino sur la concaténation de chaînes de caractères](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/concat/)
- [Article sur les bonnes pratiques pour utiliser la mémoire en Arduino](https://learn.adafruit.com/memories-of-an-arduino/anti-patterns)
- [Guide complet pour utiliser la classe String en Arduino](https://www.arduino.cc/en/Reference/String)

**N'oubliez pas de vérifier la documentation et les forums de la communauté Arduino pour trouver des solutions aux problèmes ou des astuces pour améliorer vos programmes. Bonne programmation !**