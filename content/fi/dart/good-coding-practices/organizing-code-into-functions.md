---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:13.438803-07:00
description: "Kuinka: Dartissa funktion m\xE4\xE4rittelee k\xE4ytt\xE4m\xE4ll\xE4\
  \ `void` avainsanaa, jos se ei palauta arvoa, tai m\xE4\xE4rittelem\xE4ll\xE4 sen\
  \ muutoin palautettavan arvon tyypin.\u2026"
lastmod: '2024-03-13T22:44:56.276622-06:00'
model: gpt-4-0125-preview
summary: "Dartissa funktion m\xE4\xE4rittelee k\xE4ytt\xE4m\xE4ll\xE4 `void` avainsanaa,\
  \ jos se ei palauta arvoa, tai m\xE4\xE4rittelem\xE4ll\xE4 sen muutoin palautettavan\
  \ arvon tyypin."
title: "Koodin j\xE4rjest\xE4minen funktioihin"
weight: 18
---

## Kuinka:


### Perusfunktio
Dartissa funktion määrittelee käyttämällä `void` avainsanaa, jos se ei palauta arvoa, tai määrittelemällä sen muutoin palautettavan arvon tyypin. Tässä on yksinkertainen funktio, joka tulostaa tervehdysviestin:

```dart
void greet(String name) {
  print('Hei, $name!');
}

void main() {
  greet('Alice');  // Tuloste: Hei, Alice!
}
```

### Arvon Palauttaminen
Funktiot voivat palauttaa arvoja. Seuraava esimerkki ottaa kaksi kokonaislukua syötteenä ja palauttaa niiden summan:

```dart
int add(int a, int b) {
  return a + b;
}

void main() {
  var summa = add(5, 3);
  print(summa);  // Tuloste: 8
}
```

### Nimettömät Funktiot
Dart tukee nimettömiä funktioita (tunnetaan myös lambda-lausekkeina tai sulkeumina), jotka voivat olla käteviä lyhyille, lennosta tehtäville toiminnoille. Tässä on, miten käyttää nimetöntä funktiota listan `forEach` metodilla:

```dart
void main() {
  var hedelmät = ['omena', 'banaani', 'kirsikka'];
  hedelmät.forEach((item) {
    print(item);
  });
  // Tuloste:
  // omena
  // banaani
  // kirsikka
}
```

### Nuolisyntaksi Yhden Lausekkeen Funktioille
Funktioille, jotka sisältävät vain yhden lausekkeen, Dart tarjoaa tiiviin syntaksin käyttäen "nuoli" notaatiota (`=>`). Tämä on erityisen hyödyllistä lyhyille funktioille tai funktioiden välittämiseen argumentteina:

```dart
int square(int num) => num * num;

void main() {
  print(square(4));  // Tuloste: 16
}
```

### Kolmannen Osapuolen Kirjastojen Käyttö
Monimutkaisempia tai erikoistuneempia toiminnallisuuksia varten Dart-ohjelmoijat luottavat usein kolmannen osapuolen kirjastoihin. Harkitse `http`-kirjastoa HTTP-pyyntöjen tekemiseen. Lisää ensin `http` riippuvuudeksesi pubspec.yaml-tiedostoon:

```
dependencies:
  http: ^0.13.3
```

Sen jälkeen voit käyttää sitä hakeaksesi tietoja verkosta:

```dart
import 'package:http/http.dart' as http;

Future<void> fetchUserData() async {
  var vastaus = await http.get(Uri.parse('https://api.example.com/users/1'));
  print(vastaus.body);
}

void main() {
  fetchUserData();
  // Odotettu tuloste: Käyttäjän JSON-tiedot. Todellinen tuloste riippuu API:n vastauksesta.
}
```

Muista, kun järjestät Dart-koodisi funktioihin, mieti uudelleenkäytettävyyttä, selkeyttä ja yksittäisen vastuun periaatetta. Tämä ei ainoastaan tee koodistasi siistimpää, vaan myös helpompaa muiden (ja tulevan sinun) ymmärtää ja ylläpitää.
