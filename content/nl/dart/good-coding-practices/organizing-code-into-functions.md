---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:38.711688-07:00
description: "Het organiseren van code in functies in Dart gaat over het defini\xEB\
  ren van herbruikbare blokken code die specifieke taken uitvoeren, typisch input\u2026"
lastmod: '2024-03-11T00:14:24.326998-06:00'
model: gpt-4-0125-preview
summary: "Het organiseren van code in functies in Dart gaat over het defini\xEBren\
  \ van herbruikbare blokken code die specifieke taken uitvoeren, typisch input\u2026"
title: Code organiseren in functies
---

{{< edit_this_page >}}

## Wat & Waarom?
Het organiseren van code in functies in Dart gaat over het definiëren van herbruikbare blokken code die specifieke taken uitvoeren, typisch input ontvangen, gegevens verwerken en mogelijk outputs teruggeven. Programmeurs doen dit om de leesbaarheid van de code te verbeteren, duplicatie te verminderen en onderhoud te vergemakkelijken, wat uiteindelijk leidt tot meer modulaire en beheersbare codebases.

## Hoe te:
### Basisfunctie
In Dart definieer je een functie met het trefwoord `void` als deze geen waarde teruggeeft, of je geeft anders het type waarde op dat het teruggeeft. Hier is een eenvoudige functie die een begroetingsbericht afdrukt:

```dart
void greet(String name) {
  print('Hallo, $name!');
}

void main() {
  greet('Alice');  // Output: Hallo, Alice!
}
```

### Een Waarde Retourneren
Functies kunnen waarden retourneren. Het volgende voorbeeld neemt twee integers als invoer en retourneert hun som:

```dart
int add(int a, int b) {
  return a + b;
}

void main() {
  var som = add(5, 3);
  print(som);  // Output: 8
}
```

### Anonieme Functies
Dart ondersteunt anonieme functies (ook bekend als lambda-uitdrukkingen of sluitingen), die handig kunnen zijn voor korte, ad-hoc functionaliteiten. Hier leest u hoe u een anonieme functie gebruikt met de `forEach`-methode van een lijst:

```dart
void main() {
  var fruits = ['appel', 'banaan', 'kers'];
  fruits.forEach((item) {
    print(item);
  });
  // Output:
  // appel
  // banaan
  // kers
}
```

### Pijlnotatie voor Functies met Één Expressie
Voor functies die slechts één expressie bevatten, biedt Dart een beknopte syntaxis met de "pijl"-notatie (`=>`). Dit is vooral handig voor korte functies of het doorgeven van functies als argumenten:

```dart
int square(int num) => num * num;

void main() {
  print(square(4));  // Output: 16
}
```

### Gebruik van Bibliotheken van Derden
Voor complexere of gespecialiseerde functionaliteiten, vertrouwen Dart-programmeurs vaak op bibliotheken van derden. Overweeg de `http` bibliotheek voor het maken van HTTP-verzoeken. Voeg eerst `http` toe aan je pubspec.yaml-bestand onder afhankelijkheden:

```
dependencies:
  http: ^0.13.3
```

Vervolgens kun je het gebruiken om gegevens van het web te halen:

```dart
import 'package:http/http.dart' as http;

Future<void> fetchUserData() async {
  var response = await http.get(Uri.parse('https://api.example.com/users/1'));
  print(response.body);
}

void main() {
  fetchUserData();
  // Verwachte output: JSON-gegevens van de gebruiker. De werkelijke output is afhankelijk van de reactie van de API.
}
```

Onthoud, bij het organiseren van je Dart-code in functies, denk aan herbruikbaarheid, duidelijkheid en het principe van enkele verantwoordelijkheid. Dit maakt je code niet alleen schoner, maar ook makkelijker voor anderen (en toekomstige jij) om te begrijpen en te onderhouden.
