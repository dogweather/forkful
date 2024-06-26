---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:12.234752-07:00
description: "Hvordan: I Dart definerer du en funksjon ved \xE5 bruke `void`-n\xF8\
  kkelordet hvis den ikke returnerer en verdi, eller spesifiserer typen verdi den\
  \ returnerer\u2026"
lastmod: '2024-03-13T22:44:40.493307-06:00'
model: gpt-4-0125-preview
summary: "I Dart definerer du en funksjon ved \xE5 bruke `void`-n\xF8kkelordet hvis\
  \ den ikke returnerer en verdi, eller spesifiserer typen verdi den returnerer ellers."
title: Organisering av kode i funksjoner
weight: 18
---

## Hvordan:


### Grunnleggende funksjon
I Dart definerer du en funksjon ved å bruke `void`-nøkkelordet hvis den ikke returnerer en verdi, eller spesifiserer typen verdi den returnerer ellers. Her er en enkel funksjon som skriver ut en hilsenmelding:

```dart
void greet(String name) {
  print('Hello, $name!');
}

void main() {
  greet('Alice');  // Utdata: Hello, Alice!
}
```

### Returnere en verdi
Funksjoner kan returnere verdier. Følgende eksempel tar to heltall som inndata og returnerer summen av dem:

```dart
int add(int a, int b) {
  return a + b;
}

void main() {
  var sum = add(5, 3);
  print(sum);  // Utdata: 8
}
```

### Anonyme funksjoner
Dart støtter anonyme funksjoner (også kjent som lambda-uttrykk eller lukninger), som kan være nyttige for korte, på-flyktige funksjonaliteter. Her er hvordan du bruker en anonym funksjon med en listes `forEach`-metode:

```dart
void main() {
  var frukter = ['eple', 'banan', 'kirsebær'];
  frukter.forEach((item) {
    print(item);
  });
  // Utdata:
  // eple
  // banan
  // kirsebær
}
```

### Pil-syntaks for enkeltuttrykksfunksjoner
For funksjoner som bare inneholder et enkelt uttrykk, tilbyr Dart en kortfattet syntaks ved hjelp av "pil"-notasjonen (`=>`). Dette er spesielt nyttig for korte funksjoner eller for å sende funksjoner som argumenter:

```dart
int square(int num) => num * num;

void main() {
  print(square(4));  // Utdata: 16
}
```

### Bruke tredjepartsbiblioteker
For mer komplekse eller spesialiserte funksjonaliteter, støtter Dart-programmerere ofte på tredjepartsbiblioteker. Vurder `http`-biblioteket for å gjøre HTTP-forespørsler. Først, legg til `http` i din pubspec.yaml-fil under avhengigheter:

```
dependencies:
  http: ^0.13.3
```

Deretter kan du bruke det til å hente data fra nettet:

```dart
import 'package:http/http.dart' as http;

Future<void> fetchUserData() async {
  var response = await http.get(Uri.parse('https://api.example.com/users/1'));
  print(response.body);
}

void main() {
  fetchUserData();
  // Forventet utdata: JSON-data av brukeren. Faktisk utdata vil avhenge av API-ets respons.
}
```

Husk, når du organiserer Dart-koden din i funksjoner, tenk på gjenbrukbarhet, klarhet og prinsippet om enkeltansvar. Dette gjør ikke bare koden din renere, men også lettere for andre (og fremtidige deg) å forstå og vedlikeholde.
