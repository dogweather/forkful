---
date: 2024-01-20 17:44:54.760178-07:00
description: "Extraire des sous-cha\xEEnes consiste \xE0 s\xE9lectionner des portions\
  \ sp\xE9cifiques d'une cha\xEEne de caract\xE8res. Les programmeurs le font pour\
  \ analyser, manipuler\u2026"
lastmod: '2024-03-13T22:44:58.097134-06:00'
model: gpt-4-1106-preview
summary: "Extraire des sous-cha\xEEnes consiste \xE0 s\xE9lectionner des portions\
  \ sp\xE9cifiques d'une cha\xEEne de caract\xE8res."
title: "Extraction de sous-cha\xEEnes"
weight: 6
---

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
