---
title:                "Arduino: Conversion d'une chaîne de caractères en minuscules"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous avez peut-être rencontré une situation où vous devez comparer deux chaînes de caractères, mais les utiliser telles quelles ne fonctionneraient pas. Il est nécessaire de les convertir en minuscules pour comparer leur valeur exacte. Heureusement, avec Arduino, il est facile de le faire en utilisant une simple fonction de conversion de chaîne en minuscules.

## Comment faire

Voici un exemple de code montrant comment convertir une chaîne en minuscules en utilisant la fonction `toLowerCase()` :

```
Arduino void setup() {  
  // Créer une chaîne en majuscules
  char string[] = "ARDUINO";  

  // Convertir la chaîne en minuscules
  string = toLowerCase(string);  

  // Afficher la chaîne convertie  
  Serial.println(string);  
}  

void loop() {}
```

Dans cet exemple, nous créons une chaîne en majuscules et utilisons la fonction `toLowerCase()` pour convertir cette chaîne en minuscules. Ensuite, nous l'affichons dans le moniteur série et obtiendrons le résultat suivant : "arduino".

## Plongée en profondeur

La fonction `toLowerCase()` fonctionne en parcourant chaque caractère de la chaîne et en utilisant la fonction `tolower()` pour le convertir en minuscules. Cela signifie qu'elle ne fonctionne que pour les caractères ASCII. Si votre chaîne contient des caractères accentués ou spéciaux, vous devrez peut-être utiliser une autre méthode pour les convertir en minuscules.

Il est également important de noter que la fonction `toLowerCase()` modifie la chaîne d'origine. Si vous souhaitez conserver la chaîne d'origine intacte, vous pouvez créer une copie de la chaîne et utiliser la fonction sur la copie.

## Voir aussi

- Documentation officielle sur la fonction `toLowerCase()` : https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/tolowercase/
- Exemple de code avec des caractères spéciaux : https://create.arduino.cc/projecthub/karimmufteev/arduino-strings-english-and-russian-letters-493e06
- Tutoriel sur la manipulation des chaînes de caractères : https://learn.sparkfun.com/tutorials/string-manipulation-example---arduino/all