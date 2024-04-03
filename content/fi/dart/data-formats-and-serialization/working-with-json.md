---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:19.047324-07:00
description: "Ty\xF6skentely JSONin (JavaScript Object Notation) kanssa sis\xE4lt\xE4\
  \xE4 JSON-tietojen j\xE4sent\xE4misen merkkijonoista Dart-olioihin ja p\xE4invastoin,\
  \ mik\xE4 on yleinen\u2026"
lastmod: '2024-03-13T22:44:56.294774-06:00'
model: gpt-4-0125-preview
summary: "Ty\xF6skentely JSONin (JavaScript Object Notation) kanssa sis\xE4lt\xE4\xE4\
  \ JSON-tietojen j\xE4sent\xE4misen merkkijonoista Dart-olioihin ja p\xE4invastoin,\
  \ mik\xE4 on yleinen teht\xE4v\xE4 web- ja sovelluskehityksess\xE4 tiedonvaihtoon."
title: "Ty\xF6skentely JSON:n kanssa"
weight: 38
---

## Miten:
Dart tarjoaa sisäänrakennetun tuen JSONille `dart:convert`-kirjaston avulla, mikä tekee JSONin koodaamisesta ja dekoodaamisesta suoraviivaista. Alla on esimerkkejä, jotka esittelevät perustoimintoja:

**JSON-merkkijonon jäsentäminen Dart-olioksi:**
```dart
import 'dart:convert';

void main() {
  // Esimerkki JSON-merkkijonosta
  String jsonString = '{"name": "John", "age": 30, "email": "john@example.com"}';
  
  // JSONin dekoodaus Dart Mapiksi
  Map<String, dynamic> user = jsonDecode(jsonString);
  
  print('Hei, ${user['name']}! Olet ${user['age']} vuotta vanha.');
  // Tuloste: Hei, John! Olet 30 vuotta vanha.
}
```

**Dart-olion koodaaminen JSON-merkkijonoksi:**
```dart
import 'dart:convert';

void main() {
  // Esimerkki Dart-oliosta
  Map<String, dynamic> user = {
    'name': 'Jane',
    'age': 25,
    'email': 'jane@example.com'
  };
  
  // Dart Mapin koodaus JSONiksi
  String jsonString = jsonEncode(user);
  
  print(jsonString);
  // Tuloste: {"name":"Jane","age":25,"email":"jane@example.com"}
}
```

**`json_serializable`-paketin käyttö monimutkaisissa malleissa:**
Monimutkaisten datamallien manuaalinen sarjoittaminen voi olla työlästä. `json_serializable`-paketti automatisoi tämän prosessin. Se vaatii lisäasetuksia, mukaan lukien riippuvuuksien lisäämisen `pubspec.yaml`-tiedostooon ja rakennustiedostojen luomisen. Asetusten jälkeen voit käyttää sitä seuraavasti:

1. Määrittele malli annotaatioilla:
```dart
import 'package:json_annotation/json_annotation.dart';

part 'user.g.dart';

@JsonSerializable()
class User {
  String name;
  int age;
  String email;
  
  User({required this.name, required this.age, required this.email});
  
  factory User.fromJson(Map<String, dynamic> json) => _$UserFromJson(json);
  Map<String, dynamic> toJson() => _$UserToJson(this);
}
```

2. Generoi sarjoitusnäpellys:
Käytä build runner -komentoa generoidaksesi `user.g.dart`-tiedoston:
```shell
flutter pub run build_runner build
```

3. Käytä malliasi:
```dart
void main() {
  // JSONin jäsentäminen Useriksi
  Map userMap = jsonDecode('{"name": "John", "age": 30, "email": "john@example.com"}');
  User user = User.fromJson(userMap);
  
  print('Käyttäjä: ${user.name}, Ikä: ${user.age}');
  // Tuloste: Käyttäjä: John, Ikä: 30

  // Muunna User takaisin JSONiksi
  String jsonString = jsonEncode(user.toJson());
  print(jsonString);
  // Tuloste: {"name":"John","age":30,"email":"john@example.com"}
}
```

Nämä esimerkit havainnollistavat perus- ja edistyneitä JSON-vuorovaikutuksia Dartissa, antaen kehittäjille mahdollisuuden käsitellä tiedon sarjoitustehtäviä sovelluksissaan saumattomasti.
