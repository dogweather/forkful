---
title:                "Trouver la longueur d'une chaîne"
html_title:           "Go: Trouver la longueur d'une chaîne"
simple_title:         "Trouver la longueur d'une chaîne"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & Pourquoi ?

Trouver la longueur d'une chaîne signifie compter le nombre de caractères dans une chaîne spécifique. C'est un procédé très courant en programmation, car cela aide à manipuler les chaînes de manière efficace et précise.

## Comment faire :

Voici comment trouver la longueur d'une chaîne en Arduino. Supposons que nous avons une chaîne appelée `phrase`, comme indiqué ci-dessous :

```Arduino
String phrase = "Arduino est génial";
```

Vous pouvez obtenir la longueur de `phrase` grâce à la méthode `length()`. Voici comment :

```Arduino
int longueur = phrase.length();
```

Imprimer la longueur sur le moniteur série en utilisant `Serial.println(longueur);` donnera :

```Arduino
Serial.println(longueur);
```
La sortie serait :
```Arduino
18
```
En comptant les espaces, on obtient bien 18 caractères.

## Plongée en Profondeur 

Historiquement, déterminer la longueur d'une chaîne est une technique qui a toujours été essentielle en programmation depuis les débuts des langages texte.

Il existe également des approches alternatives comme l'utilisation de boucles pour compter manuellement le nombre de caractères. Cependant, l'utilisation de la méthode `length()` est généralement plus pratique et plus efficace.

Concernant les détails d'implémentation, en Arduino, la méthode `length()` fait implicitement une importante part du travail en arrière-plan. Elle parcourt l'ensemble de la chaîne, compte chaque caractère jusqu'à ce qu'elle atteint la fin, et renvoie ce compte.

## Voir Aussi 

Pour plus d'informations sur la manipulation des chaînes en Arduino, consultez les ressources suivantes :