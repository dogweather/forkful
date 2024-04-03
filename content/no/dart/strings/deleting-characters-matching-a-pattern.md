---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:17.357849-07:00
description: "Hvordan: Dart gj\xF8r det enkelt \xE5 fjerne tegn som matcher et forh\xE5\
  ndsdefinert m\xF8nster ved bruk av regul\xE6re uttrykk og metoden `replaceAll`.\
  \ Ingen\u2026"
lastmod: '2024-03-13T22:44:40.469660-06:00'
model: gpt-4-0125-preview
summary: "Dart gj\xF8r det enkelt \xE5 fjerne tegn som matcher et forh\xE5ndsdefinert\
  \ m\xF8nster ved bruk av regul\xE6re uttrykk og metoden `replaceAll`."
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
