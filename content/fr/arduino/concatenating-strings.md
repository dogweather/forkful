---
title:    "Arduino: “Concaténation de chaînes de caractères”"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

## Pourquoi

Concaténer des chaînes de caractères est une compétence importante lorsque vous programmez en Arduino. Cela vous permet de combiner des données de différentes variables pour créer une nouvelle chaîne de caractères. Cela peut être utile pour afficher du texte sur un écran LCD ou pour envoyer des informations via une communication série. Apprendre à concaténer des chaînes de caractères vous permettra de rendre votre code plus efficace et plus propre.

## Comment faire

Voici un exemple simple pour concaténer des chaînes de caractères en utilisant Arduino :

```Arduino
String nom = "Jean";
String introduction = "Bonjour, je m'appelle ";
String phrase = introduction + nom;

Serial.println(phrase);
```

Dans cet exemple, nous utilisons la fonction ```String()``` d'Arduino pour définir trois variables : ```nom```, ```introduction``` et ```phrase```. Nous combinons ensuite les deux premières variables pour créer la variable ```phrase```. En utilisant l'opérateur de concaténation ```+```, nous ajoutons la valeur de ```nom``` à la fin de la valeur de ```introduction```. Enfin, nous imprimons la variable ```phrase``` sur le moniteur série à l'aide de la fonction ```println()```.

Lorsque vous téléversez ce code sur votre Arduino et ouvrez le moniteur série, vous devriez voir le message suivant :

```
Bonjour, je m'appelle Jean
```

Vous pouvez également concaténer des chaînes de caractères avec des valeurs numériques, comme dans cet exemple :

```Arduino
String ville = "Paris";
int population = 2;
String message = "La population de " + ville + " est de " + String(population) + " millions.";

Serial.println(message);
```

Cette fois, nous combinons une chaîne de caractères avec une variable numérique ```population```. Nous utilisons la fonction ```String()``` à nouveau pour convertir la valeur de ```population``` en une chaîne de caractères afin de pouvoir l'ajouter à notre phrase. Le moniteur série affichera alors :

```
La population de Paris est de 2 millions.
```

## Plongée en profondeur

Il existe également d'autres façons de concaténer des chaînes de caractères dans Arduino, comme l'utilisation de la fonction ```concat()``` ou l'utilisation de la bibliothèque ```String.h```. Cependant, il est important de noter que l'utilisation excessive de variables de type chaîne de caractères peut entraîner des problèmes de mémoire dans votre code.

Il est recommandé d'utiliser des chaînes de caractères avec parcimonie et de préférer d'autres types de données lorsque cela est possible.

## Voir aussi

- [Documentation officielle Arduino sur les chaînes de caractères](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [Tutoriel sur le concaténation de chaînes de caractères avec Arduino](https://circuitdigest.com/microcontroller-projects/concatenate-string-by-using-string-and-string-h-arduino-tutorial)
- [Utilisation avancée de la bibliothèque ```String.h```](https://playground.arduino.cc/Main/PrintingNumbers/)