---
title:                "Fish Shell: Trouver la longueur d'une chaîne"
simple_title:         "Trouver la longueur d'une chaîne"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Le langage de script Fish Shell est de plus en plus populaire en matière de programmation grâce à sa simplicité et à sa puissance. Une tâche courante en programmation est de trouver la longueur d'une chaîne de caractères, et dans cet article, nous allons explorer comment le faire en utilisant Fish Shell.

## Comment faire

Pour trouver la longueur d'une chaîne de caractères en Fish Shell, nous utiliserons la fonction `string length`, qui renvoie le nombre de caractères dans la chaîne donnée. Voyons un exemple concret en utilisant le code ci-dessous :

```Fish Shell
set my_string "Bonjour"
echo (string length $my_string)
```

La sortie sera `7`, car le mot "Bonjour" contient sept caractères. Nous pouvons également combiner cette fonction avec d'autres commandes, comme dans l'exemple ci-dessous où nous ajoutons la longueur de deux chaînes :

```Fish Shell
set string_1 "Hello"
set string_2 "World"
set total (string length $string_1) + (string length $string_2)
echo $total
```

La sortie sera `10`, car la longueur totale des deux chaînes se combine pour former 10 caractères.

## Profondeur d'analyse

En utilisant `string length`, il est important de noter que les espaces et autres caractères spéciaux sont pris en compte. Cela signifie que la fonction renverra la longueur complète de la chaîne, y compris les espaces. De plus, cette fonction ne peut être utilisée que sur des chaînes de caractères et non sur des nombres ou d'autres types de données.

Il est également intéressant de noter que cette fonction est sensible à la casse, ce qui signifie que les majuscules et les minuscules seront comptées comme des caractères distincts. Par exemple, la chaîne "fish" aura une longueur de 4, tandis que la chaîne "Fish" aura une longueur de 5.

## Voir aussi

- [Documentation complète sur la fonction string length](https://fishshell.com/docs/current/cmds/string_length.html)
- [Tutoriel complet sur l'utilisation de Fish Shell](https://fishshell.com/docs/current/tutorial.html)