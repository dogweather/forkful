---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:14.234012-07:00
description: "TOML, ou Tom's Obvious, Minimal Language (Langage Minimal et \xC9vident\
  \ de Tom), est un format de fichier de configuration facile \xE0 lire gr\xE2ce \xE0\
  \ sa\u2026"
lastmod: '2024-03-13T22:44:57.416672-06:00'
model: gpt-4-0125-preview
summary: "TOML, ou Tom's Obvious, Minimal Language (Langage Minimal et \xC9vident\
  \ de Tom), est un format de fichier de configuration facile \xE0 lire gr\xE2ce \xE0\
  \ sa\u2026"
title: Travailler avec TOML
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

TOML, ou Tom's Obvious, Minimal Language (Langage Minimal et Évident de Tom), est un format de fichier de configuration facile à lire grâce à sa sémantique claire. Les programmeurs l'utilisent pour configurer des applications logicielles car il est simple à analyser et génère peu de confusion ou d'erreurs.

## Comment faire :

Dart n'inclut pas de support intégré pour TOML, mais vous pouvez travailler avec des fichiers TOML en utilisant des paquets tiers comme `toml`. Tout d'abord, ajoutez `toml` à votre `pubspec.yaml` :

```yaml
dependencies:
  toml: ^0.10.0
```

### Lire TOML

Pour lire un fichier TOML, supposons que vous avez un fichier de configuration simple `config.toml` :

```toml
[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true
```

Vous pouvez analyser ce fichier TOML dans Dart comme suit :

```dart
import 'dart:io';
import 'package:toml/toml.dart';

void main() async {
  var content = await File('config.toml').readAsString();
  var doc = TomlDocument.parse(content);
  var data = doc.toMap();

  print(data['database']); // Imprime la section 'database'
}
```

Cela imprime :

```dart
{server: 192.168.1.1, ports: [8001, 8001, 8002], connection_max: 5000, enabled: true}
```

### Écrire en TOML

Pour créer du contenu TOML, utilisez le `TomlBuilder` fourni par le paquet `toml` :

```dart
import 'package:toml/toml.dart';

void main() {
  final builder = TomlBuilder();

  builder.table('database')
    ..set('server', '192.168.1.1')
    ..set('ports', [8001, 8001, 8002])
    ..set('connection_max', 5000)
    ..set('enabled', true);

  var tomlString = builder.build().toString();
  print(tomlString);
}
```

Cela va générer et imprimer une représentation sous forme de chaîne du contenu TOML, très similaire à notre fichier `config.toml` :

```toml
[database]
server = "192.168.1.1"
ports = [8001, 8001, 8002]
connection_max = 5000
enabled = true
```

Ces exemples montrent comment lire et écrire des fichiers TOML, rendant simple le travail avec des données de configuration dans vos applications Dart.
