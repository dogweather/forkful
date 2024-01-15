---
title:                "Extraction de sous-chaînes"
html_title:           "Arduino: Extraction de sous-chaînes"
simple_title:         "Extraction de sous-chaînes"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous souhaitez extraire des mots ou des caractères particuliers à partir d'une chaîne de texte, alors cet article est fait pour vous! Dans cet article, je vais vous montrer comment extraire des sous-chaînes à l'aide d'un processeur Arduino. Que vous soyez un débutant ou un utilisateur expérimenté, cette technique peut être très utile pour de nombreux projets.

## Comment faire

L'extraction de sous-chaînes consiste à sélectionner une partie spécifique d'une chaîne de caractères en utilisant des indices de début et de fin. Il existe plusieurs façons de le faire, mais voici une méthode simple en utilisant la fonction `substring()` d'Arduino. Voici un exemple de code pour extraire une sous-chaîne d'une phrase : 

```
// Déclaration de la phrase et des variables pour les indices de début et de fin
String phrase = "Bonjour, je suis un ado Arduino.";
int debut = 12;
int fin = 20;

// Utilisation de la fonction substring pour extraire la sous-chaîne 
String sous_chaine = phrase.substring(debut, fin);

// Affichage de la sous-chaîne
Serial.println(sous_chaine);
```

Résultat : "je suis un" sera imprimé dans le moniteur série. Vous pouvez également utiliser des variables pour les indices de début et de fin afin de rendre votre code plus flexible. De plus, vous pouvez également extraire plusieurs sous-chaînes à la fois en utilisant une boucle, en modifiant les valeurs des indices à chaque itération. 

## Deep Dive

Il existe d'autres fonctions utiles pour extraire des sous-chaînes sur Arduino, telles que `indexOf()` qui renvoie l'indice du premier caractère d'une sous-chaîne spécifique, et `lastIndexOf()` qui renvoie l'indice du dernier caractère d'une sous-chaîne spécifique. Vous pouvez également utiliser des conditions pour extraire des sous-chaînes en fonction de motifs spécifiques, tels que des lettres ou des mots clés.

De plus, il est important de noter que la fonction `substring()` utilise des indices en commençant à 0 pour le premier caractère de la chaîne. Cela signifie que le premier caractère a un indice de 0, le deuxième a un indice de 1, et ainsi de suite. Assurez-vous de comprendre comment les indices fonctionnent pour éviter toute confusion lors de l'extraction de sous-chaînes.

## Voir aussi

Pour en savoir plus sur les manipulations de chaînes de caractères en Arduino, vous pouvez consulter ces ressources :

- La référence d'Arduino pour la fonction `substring()` : https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/substring/
- Un tutoriel vidéo qui montre comment extraire des sous-chaînes d'un flux GPS en utilisant Arduino : https://www.youtube.com/watch?v=kl2HbyTvdBE
- Un forum d'utilisateurs Arduino où vous pouvez poser des questions sur l'extraction de sous-chaînes : https://forum.arduino.cc/index.php?topic=33526.0