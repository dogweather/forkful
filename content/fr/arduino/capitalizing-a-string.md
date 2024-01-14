---
title:    "Arduino: Mettre en majuscule une chaîne de caractères"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Pourquoi

Le fait de capitaliser une chaîne de caractères peut sembler être une tâche simple, mais c'est en réalité très utile lors de la manipulation de données dans un programme Arduino. Cela permet de rendre les données plus lisibles pour l'utilisateur et facilite leur traitement dans le code. Dans cet article, nous allons explorer comment capitaliser une chaîne de caractères en utilisant l'Arduino.

# Comment faire

Pour capitaliser une chaîne de caractères en utilisant l'Arduino, nous allons utiliser la fonction toUpperCase() de la bibliothèque String. Cette fonction prend en paramètre la chaîne de caractères que nous voulons capitaliser et renvoie une nouvelle chaîne de caractères avec toutes les lettres en majuscule.

Voici un exemple de code pour capitaliser une chaîne de caractères "arduino" et l'afficher sur le moniteur série :

```
Arduino.setup()(); // initialise la carte Arduino et le port série
String chaine = "arduino"; // création d'une chaîne de caractères
String chaineMaj = chaine.toUpperCase(); // appel de la fonction pour capitaliser la chaîne
Serial.println(chaineMaj); // affiche "ARDUINO" dans le moniteur série
```

# Plongée en profondeur

En utilisant la fonction toUpperCase(), il est important de noter que les caractères spéciaux ou accentués ne seront pas pris en compte et resteront inchangés dans la chaîne de caractères capitalisée. De plus, cette fonction ne modifie pas la chaîne d'origine, elle renvoie une nouvelle chaîne modifiée.

Il est également possible d'utiliser la fonction toUpperCase() sur un caractère unique. Dans ce cas, la fonction renverra une chaîne de caractères avec le caractère en majuscule.

# Voir aussi

- La documentation officielle de la fonction toUpperCase() de la bibliothèque String : https://www.arduino.cc/en/Reference/StringUpperCase
- Un tutoriel complet sur la manipulation de chaînes de caractères avec l'Arduino : https://www.arduino.cc/en/Tutorial/StringSubstring
- Le forum de la communauté Arduino pour échanger et poser des questions sur ce sujet : https://forum.arduino.cc/

N'hésitez pas à explorer d'autres fonctions de la bibliothèque String pour manipuler et formater vos chaînes de caractères selon vos besoins. Bonne programmation !