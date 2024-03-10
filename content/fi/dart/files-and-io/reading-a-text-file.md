---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:33.570475-07:00
description: "Tekstitiedoston lukeminen Dartilla tarkoittaa tiedostojen tietojen k\xE4\
  ytt\xE4mist\xE4 ja hakemista tiedostoj\xE4rjestelm\xE4st\xE4. Ohjelmoijat tekev\xE4\
  t t\xE4m\xE4n\u2026"
lastmod: '2024-03-09T21:06:20.196980-07:00'
model: gpt-4-0125-preview
summary: "Tekstitiedoston lukeminen Dartilla tarkoittaa tiedostojen tietojen k\xE4\
  ytt\xE4mist\xE4 ja hakemista tiedostoj\xE4rjestelm\xE4st\xE4. Ohjelmoijat tekev\xE4\
  t t\xE4m\xE4n\u2026"
title: Tekstitiedoston lukeminen
---

{{< edit_this_page >}}

## Mitä & Miksi?

Tekstitiedoston lukeminen Dartilla tarkoittaa tiedostojen tietojen käyttämistä ja hakemista tiedostojärjestelmästä. Ohjelmoijat tekevät tämän käsitelläkseen syötetietoja, konfiguraatioasetuksia tai lukeakseen datasettejä, mikä tekee siitä perustoiminnon monille sovelluksille, yksinkertaisista skripteistä monimutkaisiin appeihin.

## Kuinka:

Dartin ydinkirjasto, `dart:io`, tarjoaa tarvittavat toiminnot tekstitiedostojen lukemiseen synkronisesti tai asynkronisesti. Tässä on, miten molempia lähestytään.

**Synkronisesti:**

```dart
import 'dart:io';

void main() {
  var fileName = "polku/tekstitiedostoosi.txt";
  var tiedosto = File(fileName);

  // Luetaan tiedosto synkronisesti
  var sisalto;
  try {
    sisalto = tiedosto.readAsStringSync();
    print(sisalto);
  } catch (e) {
    print('Virhe luettaessa tiedostoa: $e');
  }
}
```

**Asynkronisesti:**

Välttääksesi ohjelman jumiutumisen tiedoston lukemisen aikana, erityisesti suurten tiedostojen tai responsiivisten sovellusten kanssa:

```dart
import 'dart:io';

void main() async {
  var fileName = "polku/tekstitiedostoosi.txt";
  var tiedosto = File(fileName);

  try {
    String sisalto = await tiedosto.readAsString();
    print(sisalto);
  } catch (e) {
    print('Virhe luettaessa tiedostoa: $e');
  }
}
```

**Esimerkkituloste:**

Jos tekstiedostosi sisältää:

```
Hei, Dart!
```

Molemmat yllämainituista menetelmistä tulostavat:

```
Hei, Dart!
```

**Kolmannen Osapuolen Kirjaston Käyttö:**

Lisäominaisuuksien, kuten yksinkertaistettujen tiedosto-operaatioiden tai parannellun virheenkäsittelyn, saamiseksi saattaisit harkita kolmannen osapuolen kirjastoja, kuten `package:file`. Kuitenkin viimeisimmän päivitykseni mukaan, ydinkirjaston `dart:io` suora käyttö, kuten yllä näytetty, on yleisin ja suoraviivaisin menetelmä tekstitiedostojen lukemiseen Dartissa.
