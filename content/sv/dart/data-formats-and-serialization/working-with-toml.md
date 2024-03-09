---
title:                "Att Arbeta med TOML"
date:                  2024-03-08T21:57:20.355843-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?

TOML, eller Toms Uppenbara, Minimala Språk, är ett konfigurationsfilformat som är lätt att läsa på grund av sin tydliga semantik. Programmerare använder det för att konfigurera mjukvaruapplikationer eftersom det är lätt att tolka och genererar minimal förvirring eller fel.

## Hur man gör:

Dart inkluderar inte inbyggt stöd för TOML, men du kan arbeta med TOML-filer med hjälp av tredjepartspaket som `toml`. Lägg först till `toml` i din `pubspec.yaml`:

```yaml
dependencies:
  toml: ^0.10.0
```

### Läsa TOML

För att läsa en TOML-fil, låt oss anta att du har en enkel konfigurationsfil `config.toml`:

```toml
[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true
```

Du kan tolka denna TOML-fil i Dart enligt följande:

```dart
import 'dart:io';
import 'package:toml/toml.dart';

void main() async {
  var innehåll = await File('config.toml').readAsString();
  var doc = TomlDocument.parse(innehåll);
  var data = doc.toMap();

  print(data['database']); // Skriv ut 'database'-sektionen
}
```

Detta skriver ut:

```dart
{server: 192.168.1.1, ports: [8001, 8001, 8002], connection_max: 5000, enabled: true}
```

### Skriva TOML

För att skapa TOML-innehåll, använd `TomlBuilder` som tillhandahålls av `toml`-paketet:

```dart
import 'package:toml/toml.dart';

void main() {
  final byggare = TomlBuilder();

  byggare.table('database')
    ..set('server', '192.168.1.1')
    ..set('ports', [8001, 8001, 8002])
    ..set('connection_max', 5000)
    ..set('enabled', true);

  var tomlString = byggare.build().toString();
  print(tomlString);
}
```

Detta kommer att generera och skriva ut en strängrepresentation av TOML-innehållet, mycket lik vår `config.toml`-fil:

```toml
[database]
server = "192.168.1.1"
ports = [8001, 8001, 8002]
connection_max = 5000
enabled = true
```

Dessa exempel visar hur man läser från och skriver till TOML-filer, vilket gör det enkelt att arbeta med konfigurationsdata i dina Dart-applikationer.
