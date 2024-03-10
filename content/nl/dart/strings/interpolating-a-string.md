---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:49.446402-07:00
description: "Stringinterpolatie is het proces van het direct invoegen van variabele\
  \ waarden in strings, vaak om betekenisvolle berichten te cre\xEBren zonder lastige\u2026"
lastmod: '2024-03-09T21:06:14.676383-07:00'
model: gpt-4-0125-preview
summary: "Stringinterpolatie is het proces van het direct invoegen van variabele waarden\
  \ in strings, vaak om betekenisvolle berichten te cre\xEBren zonder lastige\u2026"
title: Een string interpoleren
---

{{< edit_this_page >}}

## Wat & Waarom?

Stringinterpolatie is het proces van het direct invoegen van variabele waarden in strings, vaak om betekenisvolle berichten te creëren zonder lastige aaneenschakelingen. Programmeurs doen dit voor schonere, beter leesbare code en om fouten te voorkomen die gemakkelijk kunnen gebeuren bij complexe stringaaneenschakelingen.

## Hoe te:

In Dart is stringinterpolatie eenvoudig, met behulp van het `$`-symbool om uitdrukkingen direct in string literals te interpoleren:

```dart
void main() {
  String name = 'Dart';
  int year = 2023;
  // Eenvoudige variabele interpolatie
  print('Leren $name in $year!');
  // Output: Leren Dart in 2023!
  
  // Uitdrukkingen interpoleren
  print('Over twee jaar zal het ${year + 2} zijn.');
  // Output: Over twee jaar zal het 2025 zijn.
}
```

In het geval waar je meer complexe uitdrukkingen hebt of operaties binnen de string zelf wilt uitvoeren, sluit de uitdrukking in met `${}`. Dart heeft geen populaire bibliotheken van derden specifiek voor stringinterpolatie, omdat het van nature goed uitgerust is om gevarieerde en complexe scenario's te hanteren.
