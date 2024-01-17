---
title:                "Interpoler une chaîne de caractères"
html_title:           "Java: Interpoler une chaîne de caractères"
simple_title:         "Interpoler une chaîne de caractères"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi les programmeurs le font?

L'interpolation de chaîne en Java est un processus qui consiste à insérer dynamiquement des valeurs dans une chaîne de caractères prédéfinie. Les programmeurs utilisent cette technique pour rendre leurs chaînes de caractères plus dynamiques et adaptables aux différentes situations.

## Comment faire?

L'utilisation de l'opérateur ```+``` est l'une des façons les plus courantes d'interpoler des chaînes en Java. Voici un exemple de code :

    ```
    String name = "Alice";
    int age = 25;
    System.out.println("My name is " + name + " and I am " + age + " years old.");
    ```

L'exécution de ce code produira la sortie suivante :

    ```
    My name is Alice and I am 25 years old.
    ```

Vous pouvez également utiliser la méthode ```format()``` de la classe ```String``` pour interpoler des chaînes en utilisant des spécificateurs de format. Voici un exemple :

    ```
    String name = "Bob";
    double balance = 1000.50;
    System.out.format("Hello %s, your current balance is $%.2f", name, balance);
    ```

La sortie de ce code sera :

    ```
    Hello Bob, your current balance is $1000.50
    ```

## Plongeons plus loin

L'interpolation de chaîne a été introduite en Java dans la version 5 comme une alternative à la concaténation de chaînes à l'aide de l'opérateur ```+```. Les développeurs doivent choisir entre ces deux méthodes en fonction de la complexité de leur code et de leur préférence personnelle.

En plus de l'utilisation de l'opérateur ```+``` et de la méthode ```format()```, il est également possible d'interpoler des chaînes en utilisant la classe ```StringBuilder``` ou en utilisant des manipulations de chaînes à l'aide de la méthode ```replace()```.

## Voir aussi

- [Guide Java de la concaténation de chaîne](https://docs.oracle.com/javase/tutorial/java/data/converting.html)
- [Tutoriel Java de l'interpolation de chaîne](https://docs.oracle.com/javase/tutorial/java/data/strings.html)