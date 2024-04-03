---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:23.348861-07:00
description: "Comment faire : En Dart, vous pouvez utiliser diverses m\xE9thodes pour\
  \ extraire des sous-cha\xEEnes, telles que `substring()`, `split()`, et les expressions\u2026"
lastmod: '2024-03-13T22:44:57.371147-06:00'
model: gpt-4-0125-preview
summary: "En Dart, vous pouvez utiliser diverses m\xE9thodes pour extraire des sous-cha\xEE\
  nes, telles que `substring()`, `split()`, et les expressions r\xE9guli\xE8res."
title: "Extraction de sous-cha\xEEnes"
weight: 6
---

## Comment faire :
En Dart, vous pouvez utiliser diverses méthodes pour extraire des sous-chaînes, telles que `substring()`, `split()`, et les expressions régulières. Chaque méthode sert à des fins différentes et offre de la flexibilité dans la gestion des chaînes de caractères.

### Utiliser `substring()` :
La méthode `substring()` est simple. Vous spécifiez l'indice de début (et éventuellement, de fin) pour découper la chaîne.

```dart
void main() {
  String exemple = "Bonjour, Monde !";
  String resultat = exemple.substring(8, 13);
  print(resultat); // Sortie : Monde
}
```

### Utiliser `split()` :
Divisez une chaîne en une liste de sous-chaînes basée sur un motif (comme un espace ou une virgule), puis accédez à la sous-chaîne par indice.

```dart
void main() {
  String exemple = "Dart est amusant";
  List<String> parties = exemple.split(' ');
  String resultat = parties[1]; // Accès par indice
  print(resultat); // Sortie : est
}
```

### Utiliser les Expressions Régulières :
Pour des motifs complexes, la classe `RegExp` de Dart est puissante. Utilisez-la pour faire correspondre des motifs et extraire des sous-chaînes.

```dart
void main() {
  String exemple = "Email : exemple@mail.com";
  RegExp regExp = RegExp(r"\b\w+@\w+\.\w+\b");
  String email = regExp.stringMatch(exemple)!;
  print(email); // Sortie : exemple@mail.com
}
```

### Bibliothèques Tiers :
Bien que la bibliothèque standard de Dart soit assez capable, vous pourriez rencontrer des scénarios où une bibliothèque tierce pourrait simplifier votre tâche. Un choix populaire pour la manipulation de chaînes de caractères et la correspondance de motifs n'est pas spécifiquement préconisé ici étant donné que les capacités intégrées de Dart suffisent souvent. Cependant, vérifiez toujours [pub.dev](https://pub.dev) pour toute bibliothèque qui pourrait mieux répondre à vos besoins spécifiques.
