---
title:                "Utiliser des expressions régulières"
html_title:           "Arduino: Utiliser des expressions régulières"
simple_title:         "Utiliser des expressions régulières"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Qu'est-ce que c'est et pourquoi c'est important?
Les expressions régulières sont un moyen de rechercher et de manipuler des chaînes de caractères selon des motifs spécifiques. Les programmeurs les utilisent pour traiter rapidement et efficacement des données textuelles complexes, telles que des adresses email, des numéros de téléphone ou des mots de passe.

## Comment faire:
Voici un exemple de code montrant comment utiliser des expressions régulières pour vérifier si une chaîne de caractères contient une adresse email valide:

```
Arduino

#include <Regex.h>

void setup() {
  Serial.begin(9600);
}

void loop() {
  // chaîne de caractères à vérifier
  String email = "john.doe@email.com";
  
  // expression régulière pour vérifier que la chaîne contient un " @"
  Regex emailPattern("[a-z0-9._%+-]+@[a-z0-9.-]+\\.[a-z]{2,4}");

  // vérifie si la chaîne correspond au motif donné
  if (emailPattern.match(email)) {
    Serial.println("Email valide !");
  } else {
    Serial.println("Email invalide !");
  }
}
```

## Plongée en profondeur:
Les expressions régulières ont été inventées dans les années 1950 par le mathématicien Stephen Kleene pour décrire des langages formels. Aujourd'hui, elles sont utilisées dans de nombreux langages de programmation et sont souvent considérées comme un outil puissant pour la manipulation de chaînes de caractères. Alternativement, un programmeur peut utiliser des fonctions de chaînes de caractères spécifiques pour atteindre le même résultat, mais cela peut être plus fastidieux et moins efficace.

## À voir aussi:
- [Documentation officielle d'Arduino](https://www.arduino.cc/reference/en/language/functions/strings/string/)
- [Regex101 - Tester des expressions régulières en ligne](https://regex101.com/)