---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:53.074955-07:00
description: "Kuinka: Dartissa merkkijonojen interpolaatio on suoraviivaista, k\xE4\
  ytt\xE4en `$`-symbolia expressioiden interpolaatioon suoraan merkkijonoliteraaleissa."
lastmod: '2024-03-13T22:44:56.255489-06:00'
model: gpt-4-0125-preview
summary: "Dartissa merkkijonojen interpolaatio on suoraviivaista, k\xE4ytt\xE4en `$`-symbolia\
  \ expressioiden interpolaatioon suoraan merkkijonoliteraaleissa."
title: Merkkijonon interpolaatio
weight: 8
---

## Kuinka:
Dartissa merkkijonojen interpolaatio on suoraviivaista, käyttäen `$`-symbolia expressioiden interpolaatioon suoraan merkkijonoliteraaleissa:

```dart
void main() {
  String name = 'Dart';
  int year = 2023;
  // Yksinkertainen muuttujan interpolaatio
  print('Opiskelemme $namea vuonna $year!');
  // Tuloste: Opiskelemme Darttia vuonna 2023!
  
  // Ilmaisujen interpolaatio
  print('Kahden vuoden päästä se on ${year + 2}.');
  // Tuloste: Kahden vuoden päästä se on 2025.
}
```

Jos sinulla on monimutkaisempia ilmaisuja tai haluat suorittaa operaatioita itse merkkijonossa, sulje ilmaisu `${}`-merkkien sisään. Dartissa ei ole mitään suosittuja kolmannen osapuolen kirjastoja nimenomaan merkkijonojen interpolaatiota varten, koska se on varustettu natiivisti käsittelemään moninaisia ja monimutkaisia skenaarioita.
