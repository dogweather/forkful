---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:58:03.298655-07:00
description: "Kuinka: Dart itsess\xE4\xE4n ei sis\xE4ll\xE4 valmista kirjastoa kompleksiluvuille,\
  \ mik\xE4 edellytt\xE4\xE4 joko oman kompleksilukuluokan toteuttamista tai kolmannen\u2026"
lastmod: '2024-03-13T22:44:56.264000-06:00'
model: gpt-4-0125-preview
summary: "Dart itsess\xE4\xE4n ei sis\xE4ll\xE4 valmista kirjastoa kompleksiluvuille,\
  \ mik\xE4 edellytt\xE4\xE4 joko oman kompleksilukuluokan toteuttamista tai kolmannen\
  \ osapuolen kirjaston k\xE4ytt\xF6\xE4."
title: "Ty\xF6skentely kompleksilukujen kanssa"
weight: 14
---

## Kuinka:
Dart itsessään ei sisällä valmista kirjastoa kompleksiluvuille, mikä edellyttää joko oman kompleksilukuluokan toteuttamista tai kolmannen osapuolen kirjaston käyttöä. Suosittu valinta tieteellisen laskennan tehtäviin, joka sisältää tuen kompleksiluvuille, on `package:scidart`.

### Peruskompleksilukuluokan toteuttaminen
Yksinkertaisiin toimenpiteisiin voit helposti määritellä oman kompleksilukuluokan:

```dart
class Complex {
  final double real;
  final double imaginary;

  Complex(this.real, this.imaginary);

  // Kahden kompleksiluvun yhteenlasku
  Complex operator +(Complex other) {
    return Complex(real + other.real, imaginary + other.imaginary);
  }

  // Merkkijonoesitys helpottaa debuggausta
  @override
  String toString() => '${real} + ${imaginary}i';
}

void main() {
  var number1 = Complex(3, 4);
  var number2 = Complex(1, 2);

  var sum = number1 + number2;
  print(sum);  // 4.0 + 6.0i
}
```

### SciDartin käyttäminen monimutkaisempiin toimenpiteisiin
Monimutkaisemmissa toimenpiteissä tai kun suorituskyky on kriittistä, `package:scidart` tarjoaa kattavan tuen kompleksiluvuille muiden tieteellisen laskennan toiminnallisuuksien ohella. Lisää ensin SciDart pubspec.yaml-tiedostoosi:

```yaml
dependencies:
  scidart: ^0.0.1-dev.9
```

Tässä on, kuinka suorittaa perustoimintoja kompleksiluvuilla käyttäen SciDartia:

```dart
import 'package:scidart/numdart.dart';

void main() {
  // Kompleksilukujen luominen
  var complexNum1 = Complex(real: 5, imaginary: 3);
  var complexNum2 = Complex(real: 2, imaginary: 7);

  // Yhteenlasku
  var sum = complexAdd(complexNum1, complexNum2);
  
  // Kertolasku
  var product = complexMultiply(complexNum1, complexNum2);

  print('Summa: ${sum.toString()}');  // Summa: Complex(real: 7.0, imaginary: 10.0)
  print('Tulo: ${product.toString()}');  // Tulo: Complex(real: -11.0, imaginary: 41.0)
}
```

Nämä esimerkit osoittavat perusmanipuloinnin ja kompleksilukujen käytön Dartissa, sekä oman toteutuksen että SciDart-kirjaston kautta, korostaen Dart:in joustavuutta ja voimaa tieteellisen laskennan tehtävissä.
