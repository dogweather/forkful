---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:22.428065-07:00
description: "Le journalisation dans Dart fait r\xE9f\xE9rence au processus d\u2019\
  enregistrement de diff\xE9rents niveaux d\u2019information pendant l\u2019ex\xE9\
  cution d\u2019un programme. Les\u2026"
lastmod: '2024-03-13T22:44:57.397406-06:00'
model: gpt-4-0125-preview
summary: "Le journalisation dans Dart fait r\xE9f\xE9rence au processus d\u2019enregistrement\
  \ de diff\xE9rents niveaux d\u2019information pendant l\u2019ex\xE9cution d\u2019\
  un programme."
title: Journalisation
weight: 17
---

## Comment faire :
Dart comprend un mécanisme de journalisation simple via la bibliothèque `dart:developer`. Pour des besoins de journalisation plus sophistiqués, les programmeurs se tournent souvent vers des bibliothèques tierces comme `logger` et `log4dart`.

### Utiliser `dart:developer`
Ceci est adapté pour une journalisation basique, surtout pendant le développement :

```dart
import 'dart:developer';

void main() {
  log('Ceci est un message de journalisation de débogage.');
}
```

Sortie :
```
Ceci est un message de journalisation de débogage.
```

### Utiliser le paquet `logger`
Pour une solution plus complète, le paquet `logger` offre différents niveaux de journalisation (par ex., info, avertissement, erreur) et peut être formaté de manière plus lisible.

Tout d'abord, ajoutez la dépendance `logger` dans votre fichier `pubspec.yaml` :

```yaml
dependencies:
  logger: ^1.0.0
```

Ensuite, utilisez-le comme suit :

```dart
import 'package:logger/logger.dart';

var logger = Logger();

void main() {
  logger.d("Ceci est un message de débogage");
  logger.w("Ceci est un message d'avertissement");
  logger.e("Ceci est un message d'erreur");
}
```

Un exemple de sortie pourrait ressembler à ceci, avec chaque type de message formaté différemment pour une identification facile :

```
💬 Ceci est un message de débogage
⚠️ Ceci est un message d'avertissement
❗️ Ceci est un message d'erreur
```

### Utiliser le paquet `log4dart`
Pour les applications nécessitant une journalisation basée sur la configuration (similaire à Log4j), `log4dart` offre une approche familière. C'est particulièrement pratique pour les applications à grande échelle.

Assurez-vous d'inclure `log4dart` dans votre `pubspec.yaml` :

```yaml
dependencies:
  log4dart: ^2.0.0
```

Un exemple d'utilisation simple :

```dart
import 'package:log4dart/log4dart.dart';

void main() {
  final logger = LoggerFactory.getLogger("MyApp");
  logger.debug("Débogage de MyApp");
  logger.info("Message d'information");
}
```

Sortie :

```
DEBUG: Débogage de MyApp
INFO: Message d'information
```

Chacune de ces méthodes offre un niveau différent de flexibilité et de complexité, des messages de débogage simples à une journalisation complète et configurable adaptée aux besoins des applications complexes.
