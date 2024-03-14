---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:39.820232-07:00
description: "L'analyse HTML en programmation consiste \xE0 extraire des donn\xE9\
  es de documents HTML. Les programmeurs font cela pour interagir avec ou gratter\
  \ le contenu\u2026"
lastmod: '2024-03-13T22:44:57.387323-06:00'
model: gpt-4-0125-preview
summary: "L'analyse HTML en programmation consiste \xE0 extraire des donn\xE9es de\
  \ documents HTML. Les programmeurs font cela pour interagir avec ou gratter le contenu\u2026"
title: Analyse Syntaxique de HTML
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
L'analyse HTML en programmation consiste à extraire des données de documents HTML. Les programmeurs font cela pour interagir avec ou gratter le contenu web pour l'extraction d'informations, des tests ou des fins d'automatisation, même lorsque des API officielles ne sont pas disponibles.

## Comment faire :
Dart ne fournit pas de support intégré pour l'analyse HTML dans ses bibliothèques de base. Cependant, vous pouvez utiliser un package tiers comme `html` pour analyser et manipuler des documents HTML.

Tout d'abord, ajoutez le package `html` à votre fichier `pubspec.yaml` :

```yaml
dependencies:
  html: ^0.15.0
```

Puis, importez le package dans votre fichier Dart :

```dart
import 'package:html/parser.dart' show parse;
import 'package:html/dom.dart';
```

Voici un exemple basique d'analyse d'une chaîne contenant du HTML et d'extraction de données :

```dart
void main() {
  var htmlDocument = """
  <html>
    <body>
      <h1>Bonjour, Dart !</h1>
      <p>Ceci est un paragraphe dans un exemple de HTML</p>
    </body>
  </html>
  """;

  // Analyser la chaîne HTML
  Document document = parse(htmlDocument);

  // Extraction de données
  String titre = document.querySelector('h1')?.text ?? "Pas de titre trouvé";
  String paragraphe = document.querySelector('p')?.text ?? "Pas de paragraphe trouvé";

  print('Titre: $titre');
  print('Paragraphe: $paragraphe');
}
```

Sortie :

```
Titre: Bonjour, Dart !
Paragraphe: Ceci est un paragraphe dans un exemple de HTML
```

Pour interagir avec des pages web réelles, vous pourriez combiner l'analyse `html` avec des requêtes HTTP (en utilisant le package `http` pour récupérer le contenu web). Voici un rapide exemple :

Tout d'abord, ajoutez le package `http` en plus de `html` :

```yaml
dependencies:
  html: ^0.15.0
  http: ^0.13.3
```

Ensuite, récupérez et analysez une page HTML du web :

```dart
import 'package:http/http.dart' as http;
import 'package:html/parser.dart' show parse;

void main() async {
  var url = 'https://example.com';
  
  // Récupérer la page web
  var réponse = await http.get(Uri.parse(url));
  
  if (réponse.statusCode == 200) {
    var document = parse(réponse.body);

    // Supposons que la page contient des balises <h1> qui vous intéressent
    var titres = document.querySelectorAll('h1').map((e) => e.text).toList();
    
    print('Titres: $titres');
  } else {
    print('La demande a échoué avec le statut : ${réponse.statusCode}.');
  }
}
```

Note : La technique de grattage web montrée ci-dessus doit être utilisée de manière responsable et conforme aux conditions de service du site web.
