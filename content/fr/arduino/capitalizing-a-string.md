---
title:                "Majuscule d'une chaîne de caractères"
html_title:           "Arduino: Majuscule d'une chaîne de caractères"
simple_title:         "Majuscule d'une chaîne de caractères"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi 

Ecrire du code pour capitaliser une chaîne de caractères n'est peut-être pas la chose la plus excitante à faire, mais cela peut être extrêmement utile dans certaines situations ! Par exemple, si vous voulez afficher une phrase en majuscules dans un projet, ou si vous voulez vous assurer que les données que vous recevez sont toutes dans le même format, la capitalisation de la chaîne peut vous aider.

## Comment faire

Pour capitaliser une chaîne de caractères avec Arduino, vous pouvez utiliser la fonction `toupper()`. Cette fonction prend un caractère en argument et renvoie le même caractère, mais en majuscules. Vous pouvez l'utiliser dans une boucle `for` pour parcourir chaque caractère de la chaîne et utiliser la fonction `toUpperCase()` pour le capitaliser. Voici un exemple de code : 

```Arduino
String chaine = "bonjour !";
for (int i = 0; i < chaine.length(); i++) {
    char caractere = chaine.charAt(i);
    caractere = toupper(caractere);
    chaine.setCharAt(i, caractere);
}
Serial.println(chaine); // affichera "BONJOUR !"
```

## Plongée en profondeur

Il est important de noter que la fonction `toupper()` ne fonctionne que sur les caractères ASCII. Si vous voulez capitaliser des caractères accentués ou des caractères spéciaux, vous devrez utiliser une table de conversion spéciale ou une bibliothèque externe. De plus, si vous avez besoin de capitaliser plusieurs chaînes de caractères différentes, il peut être utile d'écrire une fonction réutilisable plutôt que d'écrire le même code à chaque fois.

## Voir aussi

- [La documentation officielle sur `toupper()`](https://www.arduino.cc/reference/en/language/functions/communication/lowercase-and-uppercase/)
- [Un tutoriel sur l'utilisation de `toupper()` avec les caractères Accentués](https://alydotnet.com/2016/08/17/les-caracteres-accentues-avec-arduino/)
- [La bibliothèque "String-Helpers" qui permet de capitaliser des caractères spéciaux](https://github.com/Stephane-D/String-Helpers)