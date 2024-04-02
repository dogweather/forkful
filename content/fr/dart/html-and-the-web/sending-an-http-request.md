---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:12.448167-07:00
description: "Envoyer une requ\xEAte HTTP en Dart est le processus qui initie la communication\
  \ avec un serveur web ou une API depuis une application Dart. Les programmeurs\u2026"
lastmod: '2024-03-13T22:44:57.386180-06:00'
model: gpt-4-0125-preview
summary: "Envoyer une requ\xEAte HTTP en Dart est le processus qui initie la communication\
  \ avec un serveur web ou une API depuis une application Dart. Les programmeurs\u2026"
title: "Envoyer une requ\xEAte HTTP"
weight: 44
---

## Quoi & Pourquoi ?

Envoyer une requête HTTP en Dart est le processus qui initie la communication avec un serveur web ou une API depuis une application Dart. Les programmeurs réalisent cela pour récupérer des données du web, soumettre des formulaires et interagir avec des services RESTful, ce qui en fait une opération fondamentale pour le développement d'applications web, côté serveur et mobiles en Dart.

## Comment faire :

Dart inclut le paquet `http`, une manière puissante et pratique de travailler avec des ressources HTTP. Pour commencer, incluez-le dans votre fichier pubspec.yaml :

```yaml
dependencies:
  http: ^0.13.3
```

Ensuite, importez-le dans votre code Dart pour commencer à faire des requêtes :

```dart
import 'package:http/http.dart' as http;

void main() async {
  var url = Uri.parse('https://jsonplaceholder.typicode.com/todos/1');
  var réponse = await http.get(url);

  if (réponse.statusCode == 200) {
    print('Corps de la réponse: ${réponse.body}');
  } else {
    print('Échec de la requête avec le statut : ${réponse.statusCode}.');
  }
}
```

Un exemple de sortie pour une requête réussie pourrait ressembler à ceci :

```
Corps de la réponse: {
  "userId": 1,
  "id": 1,
  "title": "delectus aut autem",
  "completed": false
}
```

Pour des requêtes plus complexes, telles que les requêtes POST avec un corps JSON, vous feriez ce qui suit :

```dart
import 'dart:convert';
import 'package:http/http.dart' as http;

void main() async {
  var url = Uri.parse('https://jsonplaceholder.typicode.com/posts');
  var réponse = await http.post(
    url,
    headers: {"Content-Type": "application/json"},
    corps: jsonEncode({
      "title": 'foo',
      "body": 'bar',
      "userId": 1,
    }),
  );

  if (réponse.statusCode == 201) {
    print('Statut de la réponse: ${réponse.statusCode}');
    print('Corps de la réponse: ${réponse.body}');
  } else {
    print('Échec de la création d’un nouveau post. Statut: ${réponse.statusCode}');
  }
}
```

Un exemple de sortie pour la requête post pourrait être :

```
Statut de la réponse: 201
Corps de la réponse: {
  "title": "foo",
  "body": "bar",
  "userId": 1,
  "id": 101
}
```

Ces exemples illustrent les requêtes HTTP GET et POST de base à l'aide du paquet `http` en Dart. Ce paquet couvre la plupart des besoins pour envoyer des requêtes HTTP, y compris des scénarios plus complexes avec des en-têtes et du contenu de corps.
