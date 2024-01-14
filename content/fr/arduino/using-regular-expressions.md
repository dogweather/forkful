---
title:    "Arduino: Utilisation des expressions régulières"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Pourquoi

Les expressions régulières sont un outil utile pour faciliter vos projets Arduino. En utilisant des expressions régulières, vous pouvez rechercher et manipuler efficacement des chaînes de caractères dans votre code. Cela peut vous aider à simplifier des tâches telles que la validation des entrées utilisateur ou la recherche de motifs spécifiques dans des données.

## Comment faire

Pour utiliser des expressions régulières dans vos projets Arduino, vous devez d'abord inclure la bibliothèque "Regex" dans votre code. Voici un exemple de code qui recherche une adresse IP valide dans une chaîne de caractères :

```Arduino
#include <Regex.h>

Regex IP_regex("[0-9]+\.[0-9]+\.[0-9]+\.[0-9]+");

String input = Serial.readString();

if(IP_regex.match(input)){
  Serial.print("Adresse IP valide trouvée : ");
  Serial.println(IP_regex.last());
}
else{
  Serial.println("Aucune adresse IP valide trouvée.");
}
```

Dans cet exemple, nous utilisons la fonction `match()` de la bibliothèque Regex pour vérifier si la chaîne de caractères saisie par l'utilisateur contient une adresse IP valide. Nous pouvons également utiliser des expressions régulières pour extraire des parties spécifiques d'une chaîne de caractères en utilisant la fonction `captured()`.

Voici un autre exemple qui extrait le nom de famille d'une chaîne de courriel :

```Arduino
#include <Regex.h>

Regex email_regex("[a-zA-Z]+\@");

String email = "johndoe@example.com";

if(email_regex.match(email)){
  Serial.print("Nom de famille trouvé : ");
  Serial.println(email_regex.captured(email));
}
else{
  Serial.println("Adresse courriel invalide.");
}
```

Ce ne sont que quelques exemples de l'utilisation des expressions régulières dans vos projets Arduino. Vous pouvez également les utiliser pour valider des numéros de téléphone, des adresses postales, ou pour rechercher des motifs spécifiques dans des données.

## Plongée en profondeur

Les expressions régulières suivent un ensemble de règles spécifiques pour correspondre à des motifs dans une chaîne de caractères. Si vous souhaitez en savoir plus sur ces règles, vous pouvez consulter des ressources telles que [cette page](https://www.regular-expressions.info/) ou [celle-ci](https://openclassrooms.com/fr/courses/1603881-apprenez-a-programmer-en-c/1609274-decouvrez-les-expressions-regulieres).

Vous pouvez également utiliser des sites tels que [Regex101](https://regex101.com/) pour tester vos expressions régulières et voir à quoi elles correspondent dans une chaîne de caractères.

## Voir aussi

- [Documentation de la bibliothèque Regex pour Arduino](https://www.arduino.cc/reference/en/libraries/regex/)
- [Tutoriel sur l'utilisation des expressions régulières dans Arduino](https://lastminuteengineers.com/regex-tutorial-arduino-esp32/)
- [Exemples de projets Arduino utilisant des expressions régulières](https://create.arduino.cc/projecthub/search?q=regex)