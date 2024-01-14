---
title:    "Arduino: Conversion d'une chaîne en minuscules"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Pourquoi

Convertir une chaîne de caractères en lettres minuscules peut être utile dans de nombreuses situations en programmation Arduino. Par exemple, si vous travaillez avec des données provenant d'un capteur ou d'un module qui utilise toujours des lettres majuscules, vous pouvez vouloir les convertir en lettres minuscules pour une meilleure lisibilité ou pour faciliter la comparaison avec d'autres valeurs.

## Comment faire

Il existe plusieurs façons de convertir une chaîne de caractères en lettres minuscules en utilisant la programmation Arduino. Nous allons vous montrer deux exemples de code simples.

```Arduino 
String texte = "ARDUINO EN FRANÇAIS"; // déclare une chaîne de caractères
texte.toLowerCase(); // convertit la chaîne en lettres minuscules
Serial.println(texte); // affiche "arduino en français" dans le moniteur série
```

Dans cet exemple, nous utilisons la méthode `toLowerCase()` sur notre chaîne de caractères pour la convertir en lettres minuscules. Ensuite, nous pouvons afficher la nouvelle chaîne dans le moniteur série.

Si vous avez plusieurs chaînes de caractères que vous voulez convertir en lettres minuscules, vous pouvez également utiliser une boucle for pour parcourir chaque caractère et le convertir un par un. Voici un exemple de code qui illustre cette méthode :

```Arduino 
String texte = "ARDUINO EN FRANÇAIS"; // déclare une chaîne de caractères
for (int i = 0; i < texte.length(); i++) { // parcourt chaque caractère de la chaîne
  char c = texte.charAt(i); // récupère le caractère à la position i
  if (c >= 'A' && c <= 'Z') { // vérifie si le caractère est une lettre majuscule
    c = c + 32; // ajoute 32 à la valeur ASCII pour le convertir en lettre minuscule
  }
  texte.setCharAt(i, c); // remplace le caractère à la position i par le nouveau caractère
}
Serial.println(texte); // affiche "arduino en français" dans le moniteur série
```

## Plongée en profondeur

Dans l'exemple précédent, nous avons utilisé la méthode `toLowerCase()` pour facilement convertir notre chaîne de caractères en lettres minuscules. Mais comment fonctionne cette méthode ? En réalité, la méthode `toLowerCase()` utilise les mêmes principes que notre deuxième exemple de code : vérifier si chaque caractère est une lettre majuscule et le convertir en lettre minuscule en ajoutant 32 à sa valeur ASCII.

Il est également important de noter qu'une fois qu'une chaîne de caractères a été créée et assignée à une variable, elle ne peut pas être modifiée directement. C'est pourquoi nous avons utilisé la méthode `setCharAt()` dans notre deuxième exemple pour modifier chaque caractère un par un.

## Voir aussi

- [Guide de référence sur les chaînes de caractères en Arduino](https://www.arduino.cc/reference/en/language/variables/data-types/string/)

- [Tutoriel vidéo : Convertir une chaîne de caractères en lettres minuscules avec Arduino](https://www.youtube.com/watch?v=DoKmOd7Z7TY)