---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:25.796801-07:00
description: "T\xE9l\xE9charger une page web implique de r\xE9cup\xE9rer le contenu\
  \ d'une page web via son URL pour le traitement ou le stockage. Les programmeurs\
  \ font cela pour\u2026"
lastmod: '2024-03-13T22:44:57.388439-06:00'
model: gpt-4-0125-preview
summary: "T\xE9l\xE9charger une page web implique de r\xE9cup\xE9rer le contenu d'une\
  \ page web via son URL pour le traitement ou le stockage."
title: "T\xE9l\xE9charger une page web"
weight: 42
---

## Comment faire :
Dart fournit le package `http`, une bibliothèque tierce populaire pour effectuer des requêtes HTTP. Voici un exemple basique de comment l'utiliser pour télécharger une page web :

D'abord, ajoutez le package `http` à votre `pubspec.yaml` :

```yaml
dependencies:
  http: ^0.13.3
```

Ensuite, importez le package et utilisez-le pour récupérer le contenu d'une page web :

```dart
import 'package:http/http.dart' as http;

Future<void> main() async {
  var url = Uri.parse('http://example.com');
  var response = await http.get(url);
  if (response.statusCode == 200) {
    print('Page téléchargée :');
    print(response.body);
  } else {
    print('Échec de la requête avec le statut : ${response.statusCode}.');
  }
}
```

**Exemple de sortie** (cela variera en fonction du contenu de la page web) :

```
Page téléchargée :
<!doctype html>
<html>
<head>
    <title>Exemple de Domaine</title>
...
</html>
```

Pour des scénarios plus complexes, comme la gestion des cookies ou la définition des en-têtes d'agents utilisateurs, vous utiliseriez le même package `http` mais avec des configurations supplémentaires pour votre requête :

```dart
import 'package:http/http.dart' as http;

Future<void> main() async {
  var headers = {
    'User-Agent': 'VotreAgentUtilisateurPersonnalisé/1.0',
    'Cookie': 'nom=valeur; nom2=valeur2',
  };
  var url = Uri.parse('http://example.com');
  var response = await http.get(url, headers: headers);

  if (response.statusCode == 200) {
    print('Page téléchargée avec des en-têtes personnalisés :');
    print(response.body);
  } else {
    print('Échec de la requête avec le statut : ${response.statusCode}.');
  }
}
```

L'utilisation d'en-têtes comme ceux-ci peut imiter plus précisément les requêtes des navigateurs, ce qui est particulièrement utile lorsqu'on traite avec des sites ayant des exigences spécifiques ou des protections contre le scraping.
