---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:51.475469-07:00
description: "Enlever les guillemets d'une cha\xEEne de caract\xE8res en Dart consiste\
  \ \xE0 retirer les marques de citation doubles (\") ou simples (') au d\xE9but et\
  \ \xE0 la fin d'une\u2026"
lastmod: '2024-03-13T22:44:57.360872-06:00'
model: gpt-4-0125-preview
summary: "Enlever les guillemets d'une cha\xEEne de caract\xE8res en Dart consiste\
  \ \xE0 retirer les marques de citation doubles (\") ou simples (') au d\xE9but et\
  \ \xE0 la fin d'une\u2026"
title: "Supprimer les guillemets d'une cha\xEEne de caract\xE8res"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Enlever les guillemets d'une chaîne de caractères en Dart consiste à retirer les marques de citation doubles (") ou simples (') au début et à la fin d'une chaîne, utile pour le nettoyage de données ou la préparation des chaînes pour un traitement ultérieur. Les programmeurs font cela pour normaliser les entrées de données, assurer l'uniformité dans le stockage des données ou lorsqu'ils interagissent avec des API qui peuvent retourner des données au format cité.

## Comment faire :
Dart offre des moyens directs pour enlever les guillemets d'une chaîne en utilisant des méthodes de chaîne intégrées, sans nécessiter de bibliothèques tierces.

### Exemple 1 : Utilisation de `replaceFirst` et `replaceAll`
Si vous traitez avec des chaînes qui commencent et se terminent par des guillemets, vous pouvez utiliser les méthodes `replaceFirst` et `replaceAll` pour les supprimer.

```dart
String quotedString = '"Bonjour, Monde!"';
String singleQuotedString = '\'Programmation Dart\'';

// Suppression des guillemets doubles
String noDoubleQuotes = quotedString.replaceFirst('"', '').replaceAll('"', '');
print(noDoubleQuotes); // Sortie : Bonjour, Monde!

// Suppression des guillemets simples
String noSingleQuotes = singleQuotedString.replaceFirst('\'', '').replaceAll('\'', '');
print(noSingleQuotes); // Sortie : Programmation Dart
```

### Exemple 2 : Utilisation de `substring`
Cette méthode est utile lorsque vous êtes sûr que les guillemets se trouvent exactement au début et à la fin de la chaîne.

```dart
String quotedString = '"Développement Flutter"';
// Vérifiez s'il commence et se termine par des guillemets avant de les retirer pour éviter les erreurs
if (quotedString.startsWith('"') && quotedString.endsWith('"')) {
  quotedString = quotedString.substring(1, quotedString.length - 1);
}
print(quotedString); // Sortie : Développement Flutter
```

### Exemple 3 : Méthode d'Extension Personnalisée
Pour plus de réutilisabilité, particulièrement si votre projet implique un retrait fréquent de guillemets, envisagez de créer une extension personnalisée sur `String`.

```dart
extension UnquoteString on String {
  String unquote() {
    var str = this;
    if (str.startsWith('"') && str.endsWith('"') || str.startsWith('\'') && str.endsWith('\'')) {
      str = str.substring(1, str.length - 1);
    }
    return str;
  }
}

void main() {
  String doubleQuoted = '"Ceci est Dart"';
  String singleQuoted = '\'Ceci est génial\'';
  print(doubleQuoted.unquote()); // Sortie : Ceci est Dart
  print(singleQuoted.unquote()); // Sortie : Ceci est génial
}
```

Ces approches devraient vous aider à supprimer efficacement les guillemets des chaînes de caractères en Dart, améliorant ainsi vos flux de travail de traitement et de préparation des données.
