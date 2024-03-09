---
title:                "Merkkijonon interpolaatio"
date:                  2024-03-08T21:54:53.074955-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## Mikä & Miksi?

Merkkijonojen interpolaatio on prosessi, jossa muuttujien arvot lisätään suoraan merkkijonoihin, usein merkityksellisten viestien luomiseksi ilman hankalia yhdistelyjä. Ohjelmoijat tekevät sen saadakseen siistimpää, luettavampaa koodia ja välttääkseen virheitä, jotka ovat tyypillisiä monimutkaisissa merkkijonojen yhdistelyissä.

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
