---
title:                "Generera slumpmässiga nummer"
date:                  2024-03-08T21:54:42.714077-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?
Att generera slumpmässiga tal i Dart involverar att skapa numeriska värden som är oförutsägbara och skiljer sig åt vid varje körning. Programmerare utnyttjar denna funktionalitet av olika anledningar, från att simulera verkliga scenarier i testmiljöer till att möjliggöra spelmekanik och säkerställa säkerhet genom slumpmässighet i kryptografiska operationer.

## Hur man gör:

Darts kärnbibliotek inkluderar stöd för att generera slumpmässiga tal med klassen `Random` som finns i `dart:math`. Här är ett grundläggande exempel:

```dart
import 'dart:math';

void main() {
  var rand = Random();
  int slumpmässigtTal = rand.nextInt(100); // Genererar ett slumpmässigt heltal mellan 0 och 99
  double slumpmässigtDubbel = rand.nextDouble(); // Genererar en slumpmässig dubbel mellan 0.0 och 1.0
  print(slumpmässigtTal);
  print(slumpmässigtDubbel);
}
```

*Exempelutdata: (Detta kommer variera varje gång det körs)*

```
23
0.6722390975465775
```

För användningsområden som kräver kryptografisk slumpmässighet, erbjuder Dart konstruktören `Random.secure`:

```dart
import 'dart:math';

void main() {
  var säkerRand = Random.secure();
  int säkertSlumpmässigtTal = säkerRand.nextInt(100);
  print(säkertSlumpmässigtTal);
}
```

*Exempelutdata: (Detta kommer variera varje gång det körs)*

```
45
```

Om du arbetar med Flutter-projekt eller behöver mer komplex slumpmässighet, kan du hitta paketet `faker` användbart för att generera ett brett utbud av slumpmässiga data, såsom namn, adresser och datum.

För att använda `faker`, lägg först till det i din `pubspec.yaml`-fil:

```yaml
dependencies:
  faker: ^2.0.0
```

Importera sedan och använd det som visat:

```dart
import 'package:faker/faker.dart';

void main() {
  final faker = Faker();
  print(faker.person.name()); // Genererar ett slumpmässigt namn
  print(faker.address.city()); // Genererar ett slumpmässigt stadsnamn
}
```

*Exempelutdata:*

```
Josie Runolfsdottir
East Lysanne
```
