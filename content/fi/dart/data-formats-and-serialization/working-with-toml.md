---
title:                "TOML:n kanssa työskentely"
date:                  2024-03-08T21:57:14.251709-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## Mikä & Miksi?

TOML, eli Tom's Obvious, Minimal Language, on konfigurointitiedostomuoto, joka on helppo lukea sen selkeän semantiikan ansiosta. Ohjelmoijat käyttävät sitä ohjelmistosovellusten konfigurointiin, koska se on suoraviivaista jäsentää ja tuottaa vähän sekaannusta tai virheitä.

## Kuinka:

Dart ei sisällä sisäänrakennettua tukea TOML:lle, mutta voit työskennellä TOML-tiedostojen kanssa käyttäen kolmannen osapuolen paketteja, kuten `toml`. Lisää ensin `toml` `pubspec.yaml`-tiedostoosi:

```yaml
dependencies:
  toml: ^0.10.0
```

### TOML-tiedoston lukeminen

Oletetaan, että sinulla on yksinkertainen konfigurointitiedosto `config.toml`:

```toml
[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true
```

Voit jäsentää tämän TOML-tiedoston Dartissa seuraavasti:

```dart
import 'dart:io';
import 'package:toml/toml.dart';

void main() async {
  var content = await File('config.toml').readAsString();
  var doc = TomlDocument.parse(content);
  var data = doc.toMap();

  print(data['database']); // Tulostaa 'database'-osion
}
```

Tämä tulostaa:

```dart
{server: 192.168.1.1, ports: [8001, 8001, 8002], connection_max: 5000, enabled: true}
```

### TOML-tiedoston kirjoittaminen

TOML-sisällön luomiseen käytä `toml`-paketin tarjoamaa `TomlBuilder`-luokkaa:

```dart
import 'package:toml/toml.dart';

void main() {
  final builder = TomlBuilder();

  builder.table('database')
    ..set('server', '192.168.1.1')
    ..set('ports', [8001, 8001, 8002])
    ..set('connection_max', 5000)
    ..set('enabled', true);

  var tomlString = builder.build().toString();
  print(tomlString);
}
```

Tämä luo ja tulostaa TOML-sisällön merkkijonomuodossa, hyvin samankaltaisen kuin meidän `config.toml`-tiedostomme:

```toml
[database]
server = "192.168.1.1"
ports = [8001, 8001, 8002]
connection_max = 5000
enabled = true
```

Nämä esimerkit näyttävät, kuinka TOML-tiedostoista voi lukea ja niihin kirjoittaa, mikä tekee konfiguraatiotietojen kanssa työskentelystä yksinkertaista Dart-sovelluksissasi.
