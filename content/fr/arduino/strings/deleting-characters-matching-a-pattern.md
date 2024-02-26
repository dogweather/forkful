---
date: 2024-01-20 17:41:30.560349-07:00
description: "Supprimer des caract\xE8res selon un motif, c'est retirer des \xE9l\xE9\
  ments de vos cha\xEEnes de texte qui correspondent \xE0 un crit\xE8re pr\xE9cis.\
  \ On le fait pour\u2026"
lastmod: '2024-02-25T18:49:54.768374-07:00'
model: gpt-4-1106-preview
summary: "Supprimer des caract\xE8res selon un motif, c'est retirer des \xE9l\xE9\
  ments de vos cha\xEEnes de texte qui correspondent \xE0 un crit\xE8re pr\xE9cis.\
  \ On le fait pour\u2026"
title: "Suppression de caract\xE8res correspondant \xE0 un motif"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
Supprimer des caractères selon un motif, c'est retirer des éléments de vos chaînes de texte qui correspondent à un critère précis. On le fait pour nettoyer des données, simplifier le traitement de texte, et améliorer l'efficacité du code.

## Comment faire :
```Arduino
void setup() {
  Serial.begin(9600); // Initialiser le port série
  String message = "Bonjour, je suis un Ardu1n0!";
  String motif = "1n0";
  
  message = supprimerMotif(message, motif);
  Serial.println(message); // Affichera "Bonjour, je suis un Ardu!"
}

void loop() {
  // rien ici
}

// Fonction pour supprimer le motif dans la chaîne
String supprimerMotif(String source, String motif) {
  int index = source.indexOf(motif); // Trouve l'index du motif
  while (index >= 0) {
    source.remove(index, motif.length()); // Supprime le motif
    index = source.indexOf(motif); // Cherche à nouveau pour d'autres occurrences
  }
  return source;
}
```

## Plongée Profonde
Historiquement, supprimer des caractères dans les chaînes a toujours été une fondation de la programmation. En Arduino, il n'existe pas de méthode intégrée pour supprimer tous les caractères correspondant à un motif, donc on crée soi-même cette logique. D'autres langages offrent des expressions régulières (regex) pour accomplir ceci rapidement, mais la plateforme Arduino favorise des fonctions plus simples par souci d'économie de mémoire. En ce qui concerne l'implémentation, utilisez `String.indexOf()` pour trouver l'emplacement du motif et `String.remove()` pour le supprimer.

## À Voir Aussi
- Documentation Arduino sur [String Class](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- Tutoriel [Arduino String Manipulation](https://www.arduino.cc/en/Tutorial/BuiltInExamples/StringAdditionOperator)
- Explications sur les [expressions régulières (regex)](https://www.regular-expressions.info/) et pourquoi elles ne sont pas idéales pour Arduino en raison de l'utilisation mémoire importante.
