---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:05.260213-07:00
description: "Comment faire : Dart utilise la classe `RegExp` pour les expressions\
  \ r\xE9guli\xE8res. Voici un exemple de base pour correspondre \xE0 un motif simple\
  \ au sein\u2026"
lastmod: '2024-03-13T22:44:57.372418-06:00'
model: gpt-4-0125-preview
summary: "Dart utilise la classe `RegExp` pour les expressions r\xE9guli\xE8res."
title: "Utiliser des expressions r\xE9guli\xE8res"
weight: 11
---

## Comment faire :
Dart utilise la classe `RegExp` pour les expressions régulières. Voici un exemple de base pour correspondre à un motif simple au sein d'une chaîne :

```dart
void main() {
  var pattern = RegExp(r'\bDart\b');
  var text = 'Apprendre la programmation Dart est excitant.';

  if (pattern.hasMatch(text)) {
    print('Correspondance trouvée !');
  } else {
    print('Aucune correspondance trouvée.');
  }
  // Sortie : Correspondance trouvée !
}
```

Pour extraire les correspondances d'une chaîne, vous pouvez utiliser la méthode `allMatches`. Cette méthode retourne un itérable de correspondances :

```dart
void main() {
  var pattern = RegExp(r'\b\w+\b');
  var text = 'Dart est génial !';

  var matches = pattern.allMatches(text);
  for (final match in matches) {
    print(match.group(0)); // Cela imprime les sous-chaînes correspondantes.
  }
  // Sortie :
  // Dart
  // est
  // génial
}
```

Remplacer du texte peut être réalisé à l'aide des méthodes `replaceFirst` ou `replaceAll` :

```dart
void main() {
  var pattern = RegExp(r'\bDart\b');
  var text = 'Dart n'est pas juste un dart.';
  
  // Remplace la première occurrence
  var modifiedText = text.replaceFirst(pattern, 'Flutter');
  print(modifiedText); 
  // Sortie: Flutter n'est pas juste un dart.

  // Remplace toutes les occurrences
  modifiedText = text.replaceAll(pattern, 'Flutter');
  print(modifiedText);
  // Sortie: Flutter n'est pas juste un flutter.
}
```

Diviser une chaîne par un motif regex est simple en utilisant la méthode `split` :

```dart
void main() {
  var pattern = RegExp(r'\s+'); // Correspond à tout caractère d'espace blanc
  var text = 'Dart est amusant';

  var parts = text.split(pattern);
  print(parts); 
  // Sortie : [Dart, est, amusant]
}
```

Pour un parsing complexe ou des validations non directement supportées par `RegExp` de Dart, vous pourriez envisager des bibliothèques tierces, mais la bibliothèque standard de Dart est souvent suffisante pour les tâches regex communes, soulignant son utilité et sa polyvalence dans la gestion des expressions régulières.
