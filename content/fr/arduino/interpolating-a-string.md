---
title:                "Interpoler une chaîne de caractères"
html_title:           "Arduino: Interpoler une chaîne de caractères"
simple_title:         "Interpoler une chaîne de caractères"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/interpolating-a-string.md"
---

{{< edit_this_page >}}

# Qu'est-ce que c'est et pourquoi?
Interpoler une chaîne de caractères est une méthode de programmation qui permet de remplacer des valeurs variables dans une chaîne de caractères prédéfinie. Cela peut être utile pour afficher des messages dynamiques ou pour simplifier le code en évitant des répétitions.

# Comment faire:
Voici un exemple de code pour interpoler une chaîne de caractères dans Arduino:

```
int age = 23;  // Définir une valeur variable
String message = "J'ai {age} ans.";  // Définir une chaîne de caractères avec un espace réservé pour la variable

// Utiliser la fonction « replace() » pour remplacer l'espace réservé par la valeur de la variable
message.replace("{age}", String(age));

// Afficher le résultat sur le moniteur série
Serial.println(message);
```

La sortie dans le moniteur série sera : ```J'ai 23 ans.```

# Plongée en profondeur:
L'interpolation de chaîne de caractères est également connue sous le nom de « formatage de chaîne de caractères ». Elle est souvent utilisée dans les langages de programmation pour simplifier la manipulation de chaînes de caractères. D'autres techniques pour remplacer des valeurs variables dans une chaîne de caractères incluent l'utilisation de « sprintf() » ou de « snprintf() ».

# Voir aussi:
Pour en savoir plus sur l'interpolation de chaîne de caractères dans le contexte d'Arduino, consultez la documentation officielle : https://www.arduino.cc/reference/en/language/structure/strings/stringinterpolation/

Vous pouvez également apprendre davantage sur les différentes méthodes pour manipuler des chaînes de caractères dans Arduino ici : https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/