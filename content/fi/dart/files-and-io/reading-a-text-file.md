---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:33.570475-07:00
description: "Kuinka: Dartin ydinkirjasto, `dart:io`, tarjoaa tarvittavat toiminnot\
  \ tekstitiedostojen lukemiseen synkronisesti tai asynkronisesti. T\xE4ss\xE4 on,\
  \ miten\u2026"
lastmod: '2024-03-13T22:44:56.290332-06:00'
model: gpt-4-0125-preview
summary: Dartin ydinkirjasto, `dart:io`, tarjoaa tarvittavat toiminnot tekstitiedostojen
  lukemiseen synkronisesti tai asynkronisesti.
title: Tekstitiedoston lukeminen
weight: 22
---

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
