---
title:                "Organiser le code en fonctions"
date:                  2024-03-08T21:55:06.855592-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Organiser le code en fonctions dans Dart consiste à définir des blocs de code réutilisables qui effectuent des tâches spécifiques, recevant généralement des entrées, traitant des données et, éventuellement, retournant des sorties. Les programmeurs font cela pour améliorer la lisibilité du code, réduire la duplication et faciliter la maintenance, conduisant finalement à des bases de code plus modulaires et gérables.

## Comment faire :
### Fonction Basique
Dans Dart, vous définissez une fonction en utilisant le mot-clé `void` si elle ne retourne pas de valeur, ou vous spécifiez le type de valeur qu'elle retourne autrement. Voici une fonction simple qui imprime un message de salutation :

```dart
void greet(String name) {
  print('Bonjour, $name!');
}

void main() {
  greet('Alice');  // Sortie : Bonjour, Alice!
}
```

### Retourner une Valeur
Les fonctions peuvent retourner des valeurs. L'exemple suivant prend deux entiers en entrée et retourne leur somme :

```dart
int add(int a, int b) {
  return a + b;
}

void main() {
  var sum = add(5, 3);
  print(sum);  // Sortie : 8
}
```

### Fonctions Anonymes
Dart prend en charge les fonctions anonymes (également connues sous le nom d'expressions lambda ou de fermetures), qui peuvent être utiles pour des fonctionnalités courtes et improvisées. Voici comment utiliser une fonction anonyme avec la méthode `forEach` d'une liste :

```dart
void main() {
  var fruits = ['pomme', 'banane', 'cerise'];
  fruits.forEach((item) {
    print(item);
  });
  // Sortie :
  // pomme
  // banane
  // cerise
}
```

### Syntaxe Fléchée pour les Fonctions à Une Seule Expression
Pour les fonctions qui ne contiennent qu'une seule expression, Dart offre une syntaxe concise en utilisant la notation "flèche" (`=>`). Ceci est particulièrement utile pour les fonctions courtes ou pour passer des fonctions en tant qu'arguments :

```dart
int square(int num) => num * num;

void main() {
  print(square(4));  // Sortie : 16
}
```

### Utilisation de Bibliothèques Tierces
Pour des fonctionnalités plus complexes ou spécialisées, les programmeurs Dart comptent souvent sur des bibliothèques tierces. Prenez en considération la bibliothèque `http` pour effectuer des requêtes HTTP. Tout d'abord, ajoutez `http` à votre fichier pubspec.yaml sous les dépendances :

```
dependencies:
  http: ^0.13.3
```

Ensuite, vous pouvez l'utiliser pour récupérer des données du Web :

```dart
import 'package:http/http.dart' as http;

Future<void> fetchUserData() async {
  var response = await http.get(Uri.parse('https://api.example.com/users/1'));
  print(response.body);
}

void main() {
  fetchUserData();
  // Sortie attendue : Données JSON de l'utilisateur. La sortie réelle dépendra de la réponse de l'API.
}
```

Rappelez-vous, lors de l'organisation de votre code Dart en fonctions, pensez à la réutilisabilité, à la clarté et au principe de responsabilité unique. Cela rend non seulement votre code plus propre, mais aussi plus facile à comprendre et à maintenir pour les autres (et pour vous dans le futur).
