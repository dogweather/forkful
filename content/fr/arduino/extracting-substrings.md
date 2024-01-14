---
title:    "Arduino: Extraction de sous-chaînes"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

# Pourquoi

La manipulation des chaînes de caractères est un élément essentiel de la programmation arduino. L'extraction de sous-chaînes est particulièrement utile lorsqu'on a besoin de traiter des données spécifiques à partir d'une chaîne plus grande. Cela peut être particulièrement utile pour extraire des informations des capteurs ou des données envoyées par un utilisateur.

## Comment faire

L'extraction de sous-chaînes peut être réalisée en utilisant la fonction `substring()` en arduino. Voici un exemple de code qui extrait une sous-chaîne à partir d'une chaîne définie :

```Arduino
String phrase = "Bonjour tout le monde";
String sousPhrase = phrase.substring(8, 15);

Serial.println(sousPhrase);
```

Dans cet exemple, nous avons créé une chaîne de caractères appelée "phrase" et lui avons donné la valeur "Bonjour tout le monde". Nous avons ensuite utilisé la fonction `substring()` avec les paramètres 8 et 15 pour extraire une sous-chaîne à partir de la position 8 (incluse) jusqu'à la position 15 (non incluse). La sous-chaîne extraite sera donc "tout le".

Nous pouvons également utiliser cette fonction pour extraire une sous-chaîne à partir d'une position spécifique jusqu'à la fin de la chaîne en n'indiquant qu'un seul paramètre :

```Arduino
String phrase = "Bonjour tout le monde";
String sousPhrase = phrase.substring(8);

Serial.println(sousPhrase);
```

Ici, nous obtenons comme résultat "tout le monde" car nous avons indiqué la position 8 jusqu'à la fin de la chaîne.

## Plongée en profondeur

La fonction `substring()` peut également prendre en compte des valeurs négatives en spécifiant une position relative à la fin de la chaîne. Par exemple, si nous voulons extraire les trois derniers caractères d'une chaîne, nous pouvons utiliser les valeurs -3 et -1 comme paramètres :

```Arduino
String phrase = "Bonjour tout le monde";
String sousPhrase = phrase.substring(-3, -1);

Serial.println(sousPhrase);
```

Cela nous donnera la sous-chaîne "de" car nous avons indiqué la position -3 comme inclus et la position -1 comme non inclus.

Il est également possible d'utiliser la fonction `substring()` pour extraire une partie d'une sous-chaîne. Par exemple, si nous avons une sous-chaîne "tout le" et que nous voulons extraire seulement les deux premiers caractères, nous pouvons le faire en utilisant les valeurs 0 et 2 comme paramètres :

```Arduino
String sousPhrase = "tout le";
String nouvelleSousPhrase = sousPhrase.substring(0, 2);

Serial.println(nouvelleSousPhrase);
```

Cela nous donnera comme résultat "to" car nous avons indiqué la position 0 comme incluse et la position 2 comme non incluse.

# Voir aussi

Vous pouvez consulter les liens suivants pour en savoir plus sur l'extraction de sous-chaînes en arduino :

- [Documentation officielle de la fonction `substring()` en arduino](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/substring/)
- [Tutoriel sur l'extraction de sous-chaînes en arduino](https://www.robocircuits.com/arduino-strings-explained-and-examples/)
- [Forum de la communauté arduino pour poser vos questions et en savoir plus](https://forum.arduino.cc/)

Maintenant que vous savez comment extraire des sous-chaînes en arduino, vous pourrez facilement manipuler et traiter des données spécifiques pour vos projets. Bonne programmation !