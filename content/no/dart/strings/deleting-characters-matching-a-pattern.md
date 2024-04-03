---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:17.357849-07:00
description: "\xC5 slette tegn som samsvarer med et spesifikt m\xF8nster i strenger\
  \ er avgj\xF8rende for datakontroll, sanitering eller n\xE5r man forbereder tekst\
  \ til videre\u2026"
lastmod: '2024-03-13T22:44:40.469660-06:00'
model: gpt-4-0125-preview
summary: "\xC5 slette tegn som samsvarer med et spesifikt m\xF8nster i strenger er\
  \ avgj\xF8rende for datakontroll, sanitering eller n\xE5r man forbereder tekst til\
  \ videre behandling."
title: "Slette tegn som samsvarer med et m\xF8nster"
weight: 5
---

## Hvordan:
Dart gjør det enkelt å fjerne tegn som matcher et forhåndsdefinert mønster ved bruk av regulære uttrykk og metoden `replaceAll`. Ingen tredjepartsbiblioteker er nødvendig for grunnleggende bruk, noe som gjør denne tilnærmingen veldig tilgjengelig.

Her er et enkelt eksempel som demonstrerer hvordan du fjerner sifre fra en streng:

```dart
void main() {
  String stringWithDigits = 'Dart123 er gøy456';
  // Definer et regulært uttrykksmønster som matcher alle sifre
  RegExp digitPattern = RegExp(r'\d');
  
  // Erstatt alle forekomster av mønsteret med en tom streng
  String result = stringWithDigits.replaceAll(digitPattern, '');
  
  print(result); // Utdata: Dart er gøy
}
```

Anta at du står overfor et mer komplekst scenario, som å fjerne spesialtegn unntatt mellomrom og punktuering. Her er hvordan du ville gjort det:

```dart
void main() {
  String messyString = 'Dart!@# er *&()gøy$%^';
  // Definer et mønster som matcher alt unntatt bokstaver, tall, mellomrom og punktuering
  RegExp specialCharPattern = RegExp(r'[^a-zA-Z0-9 \.,!?]');
  
  String cleanedString = messyString.replaceAll(specialCharPattern, '');
  
  print(cleanedString); // Utdata: Dart! er gøy
}
```

For oppgaver som krever mer avansert mønstermatching og erstatning, tilbyr Darts omfattende `RegExp` klasse dokumentasjon en dybdeinnsikt i mer komplekse uttrykk og deres bruk. Imidlertid dekker de ovennevnte eksemplene de fleste vanlige bruksområder for å slette tegn basert på mønstre i Dart-programmering.
