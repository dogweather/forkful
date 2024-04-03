---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:58:14.253875-07:00
description: "Testien kirjoittaminen Dartissa sis\xE4lt\xE4\xE4 testitapausten luomisen,\
  \ joiden avulla automaattisesti varmistetaan, ett\xE4 ohjelman eri osat toimivat\
  \ odotetusti.\u2026"
lastmod: '2024-03-13T22:44:56.274580-06:00'
model: gpt-4-0125-preview
summary: "Testien kirjoittaminen Dartissa sis\xE4lt\xE4\xE4 testitapausten luomisen,\
  \ joiden avulla automaattisesti varmistetaan, ett\xE4 ohjelman eri osat toimivat\
  \ odotetusti."
title: Testien kirjoittaminen
weight: 36
---

## Mitä & Miksi?

Testien kirjoittaminen Dartissa sisältää testitapausten luomisen, joiden avulla automaattisesti varmistetaan, että ohjelman eri osat toimivat odotetusti. Ohjelmoijat tekevät tämän varmistaakseen, että heidän koodinsa on luotettavaa ja vapaa puutteista, mikä helpottaa päivityksiä ja refaktorointia estäen samalla regressiot.

## Kuinka:

Dartissa `test`-pakettia käytetään yleisesti testien kirjoittamiseen. Lisää ensin `test`-paketti `pubspec.yaml`-tiedostoosi:

```yaml
dev_dependencies:
  test: ^1.0.0
```

Kirjoita sitten testi yksinkertaiselle funktiolle. Oletetaan, että sinulla on funktio, joka laskee yhteen kaksi lukua:

```dart
int add(int a, int b) {
  return a + b;
}
```

Seuraavaksi, luo tiedosto nimeltä `add_test.dart` `test`-hakemistoon ja kirjoita testitapauksesi:

```dart
import 'package:test/test.dart';
import '../lib/add.dart'; // Oletetaan, että `add`-funktiosi on lib/add.dart-tiedostossa

void main() {
  test('laskee kaksi lukua yhteen', () {
    var odotettu = 3;
    expect(add(1, 2), equals(odotettu));
  });
}
```

Testien suorittamiseen käytä Dart-komentoa:

```bash
$ dart test
```

Esimerkkitulostus voi näyttää tältä:

```
00:01 +1: Kaikki testit läpäisty!
```

### Kolmannen osapuolen kirjaston käyttö: Mockito mock-olioiden luomiseen

Monimutkaisten riippuvuuksien koodin testaamisessa saatat käyttää Mockitoa mock-olioiden luomiseen. Lisää ensin Mockito `pubspec.yaml`-tiedostoosi:

```yaml
dev_dependencies:
  mockito: ^5.0.0
```

Oletetaan, että sinulla on luokka `UserRepository`, joka noutaa käyttäjätietoja, ja haluat testata `UserService`-palvelua, joka riippuu `UserRepository`-palvelusta ilman oikean tietokannan käyttöä:

```dart
import 'package:mockito/mockito.dart';
import 'package:test/test.dart';
import 'package:your_project/user_repository.dart';
import 'package:your_project/user_service.dart';

// Luo Mock-luokka käyttäen Mockitoa
class MockUserRepository extends Mock implements UserRepository {}

void main() {
  group('UserService Testit', () {
    test('Noutaa käyttäjän onnistuneesti', () {
      // Luo mock-instanssi
      final mockUserRepository = MockUserRepository();
      final userService = UserService(mockUserRepository);

      // Asetetaan mock-käyttäytymistä
      when(mockUserRepository.fetchUser(1)).thenReturn(User(id: 1, name: 'Testikäyttäjä'));

      // Varmistetaan, että mock-metodia kutsutaan odotetuilla argumenteilla
      expect(userService.getUserName(1), 'Testikäyttäjä');
      verify(mockUserRepository.fetchUser(1)).called(1);
    });
  });
}
```

Tämän testin suorittaminen vahvistaa, että `UserService` vuorovaikuttaa oikein `UserRepository`-palvelun kanssa käyttäen mockausmenetelmää todellisten vuorovaikutusten simuloimiseksi kontrolloidulla tavalla.
