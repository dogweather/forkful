---
title:                "Recherche et remplacement de texte"
html_title:           "Arduino: Recherche et remplacement de texte"
simple_title:         "Recherche et remplacement de texte"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & Pourquoi ?
La recherche et le remplacement de texte sont des opérations qui permettent de trouver une chaîne de caractères spécifique dans un texte et de la remplacer par une autre. Les programmeurs le font pour manipuler et transformer des données textuelles afin de répondre à divers besoins fonctionnels.

## Comment faire :
Voici un exemple simple de recherche et de remplacement sur Arduino :

```Arduino
String texte = "Bonjour le monde";
texte.replace("monde", "Arduino");
// Affiche : "Bonjour le Arduino"
Serial.println(texte);
```

Dans cet exemple, la méthode `replace` de la classe `String` est utilisée pour remplacer le mot "monde" par "Arduino" dans une chaîne de caractères.

## Plongée profonde
Historiquement, la recherche et le remplacement de texte ont été une caractéristique importante des éditeurs de texte, permettant aux utilisateurs de faire de larges modifications sans effort et sans erreurs. En Arduino, cela est rendu possible grâce à la classe `String`.

Il existe des alternatives à la méthode `replace` d'Arduino, telles que l'utilisation des fonctions `strstr` et `strcpy` du langage C. Cependant, la méthode `replace` offre une interface plus simple et plus conviviale.

Dans le détail, la méthode `replace` de la classe `String` prend deux arguments: la chaîne de caractères à rechercher et la chaîne de caractères pour la remplacer. Elle parcourt ensuite le texte, identifie tous les cas de la chaîne de recherche, et les remplace par la nouvelle chaîne, en respectant l'ordre d'occurrence.

## Voir aussi
Pour une understanding plus approfondie, consultez ces liens :
- [Documentation de la classe String sur Arduino](https://www.arduino.cc/reference/tr/stringobject/)
- [Fonctions de manipulation de chaînes en C (en anglais)](https://www.tutorialspoint.com/c_standard_library/c_function_strstr.htm)
- [Fonctions de manipulation de chaînes en C++ (en anglais)](https://www.cplusplus.com/reference/cstring/)