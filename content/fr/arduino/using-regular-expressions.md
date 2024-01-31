---
title:                "Utilisation des expressions régulières"
date:                  2024-01-19
html_title:           "Arduino: Utilisation des expressions régulières"
simple_title:         "Utilisation des expressions régulières"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
Utiliser des expressions régulières, c’est chercher des patterns dans du texte. Les programmeurs s'en servent pour valider, rechercher et manipuler des données de façon efficace.

## How to:
Arduino n’a pas de support natif pour les expressions régulières, mais on peut utiliser la bibliothèque `Regexp` disponible via le Gestionnaire de bibliothèque. Voici comment matcher un motif simple.

```Arduino
#include <Regexp.h>

void setup() {
  Serial.begin(9600);

  // Crée un objet MatchState
  MatchState ms;
  // La chaîne à analyser
  char input[] = "Ceci est un test 1234.";
  
  // La pattern pour matcher des chiffres
  ms.Target(input);
  char result = ms.Match ("[0-9]+");

  if (result > 0) {
    char match[100];
    ms.GetMatch(match, 0);
    Serial.println(match); // Affiche les chiffres trouvés
  } else {
    Serial.println("Pas de chiffres trouvés.");
  }
}

void loop() {
  // Ici, le contenu ne change pas
}
```

Exemple de sortie:

```
1234
```

## Deep Dive
Les expressions régulières existent depuis les années 1950. Arduino, manquant d'opérations sur les chaînes de caractères complexes, ne les intègre pas nativement. Les bibliothèques comme `Regexp` offrent une solution. Elles ne sont pas aussi performantes que celles intégrées dans des langages tels que Python ou JavaScript, mais elles suffisent pour des tâches simples en embarqué.

## See Also
- [Documentation Arduino sur les strings](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [Bibliothèque `Regexp` pour Arduino](https://github.com/nickgammon/Regexp)
- [Tutorial sur les expressions régulières](http://www.regular-expressions.info/tutorial.html)
