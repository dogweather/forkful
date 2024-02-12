---
title:                "Extraction de sous-chaînes"
aliases:
- /fr/arduino/extracting-substrings/
date:                  2024-01-20T17:44:54.760178-07:00
model:                 gpt-4-1106-preview
simple_title:         "Extraction de sous-chaînes"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi et Pourquoi?)
Extraire des sous-chaînes consiste à sélectionner des portions spécifiques d'une chaîne de caractères. Les programmeurs le font pour analyser, manipuler ou tester des données de texte.

## How to: (Comment faire:)
```Arduino
void setup() {
  Serial.begin(9600);

  String phrase = "Bonjour, monde!";
  String mot = phrase.substring(0, 7);  // Extrait "Bonjour"

  Serial.println(mot);
}

void loop() {
  // Rien à boucler ici.
}
```
Sortie: `Bonjour`

```Arduino
void setup() {
  Serial.begin(9600);

  String phrase = "Nuit étoilée sur le Rhône";
  String extrait = phrase.substring(17);  // Extrait "sur le Rhône"

  Serial.println(extrait);
}

void loop() {
  // Rien à boucler ici.
}
```
Sortie: `sur le Rhône`

## Deep Dive (Plongeon en profondeur)
Historiquement, les langages de programmation intègrent souvent des fonctionnalités pour manipuler des chaînes de caractères, réfléchissant le besoin fréquent de gérer du texte. En Arduino, `String.substring()` est facile d'utilisation, mais gare à la gestion mémoire : les `String` peuvent fragmenter la RAM. Alternatives incluent `strncpy` de C ou utiliser des pointeurs pour une approche plus bas-niveau et contrôlée. L'implémentation derrière `substring` alloue une nouvelle `String`, donc il est crucial de libérer la mémoire si elle n'est plus nécessaire.

## See Also (Voir Aussi)
- Documentation Arduino sur `String`: [Arduino Reference: String](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- Guide Arduino pour gérer la mémoire: [Memory](https://www.arduino.cc/en/Tutorial/Memory)
- Tutoriel sur le traitement des chaînes en C: [C String Handling](https://www.tutorialspoint.com/cprogramming/c_strings.htm)
