---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:16.464117-07:00
description: "Envoyer une requ\xEAte HTTP avec une authentification de base implique\
  \ l'ajout d'un nom d'utilisateur et d'un mot de passe \xE0 une requ\xEAte pour v\xE9\
  rifier\u2026"
lastmod: '2024-03-13T22:44:57.389563-06:00'
model: gpt-4-0125-preview
summary: "Envoyer une requ\xEAte HTTP avec une authentification de base implique l'ajout\
  \ d'un nom d'utilisateur et d'un mot de passe \xE0 une requ\xEAte pour v\xE9rifier\
  \ l'identit\xE9 de l'utilisateur."
title: "Envoyer une requ\xEAte HTTP avec une authentification de base"
weight: 45
---

## Quoi & Pourquoi ?

Envoyer une requête HTTP avec une authentification de base implique l'ajout d'un nom d'utilisateur et d'un mot de passe à une requête pour vérifier l'identité de l'utilisateur. Les programmeurs utilisent cette méthode pour accéder à des ressources nécessitant une authentification, assurant ainsi une communication sécurisée entre le client et le serveur.

## Comment faire :

En Dart, vous pouvez utiliser le package `http` pour envoyer des requêtes HTTP avec une authentification de base. Tout d'abord, ajoutez le package `http` à votre fichier `pubspec.yaml` :

```yaml
dependencies:
  http: ^0.13.4
```

Ensuite, importez le package dans votre fichier Dart :

```dart
import 'package:http/http.dart' as http;
import 'dart:convert';
```

Pour envoyer une requête GET avec une authentification de base, vous pouvez utiliser le code suivant :

```dart
Future<void> fetchUserData() async {
  final username = 'yourUsername';
  final password = 'yourPassword';
  final credentials = base64Encode(utf8.encode('$username:$password'));
  final response = await http.get(
    Uri.parse('https://yourapi.com/userdata'),
    headers: {
      'Authorization': 'Basic $credentials',
    },
  );

  if (response.statusCode == 200) {
    print('Données utilisateur récupérées avec succès !');
    print('Corps de la réponse : ${response.body}');
  } else {
    print('Échec de la récupération des données utilisateur avec le code d’état : ${response.statusCode}');
  }
}
```

Ce code envoie une requête GET à 'https://yourapi.com/userdata' avec un en-tête d'authentification de base. Le nom d'utilisateur et le mot de passe sont encodés en base64 et passés dans l'en-tête 'Authorization' conformément aux normes d'authentification d'accès de base.

**Exemple de sortie :**

Après une requête réussie et si le serveur renvoie un code d'état 200, vous pourriez voir :

```plaintext
Données utilisateur récupérées avec succès !
Corps de la réponse : {"id":1, "name":"John Doe", "email":"john@example.com"}
```

Si l'authentification échoue ou s'il y a une autre erreur, le code d'état de la réponse aidera à identifier le problème.
