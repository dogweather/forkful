---
title:                "Trabajando con TOML"
date:                  2024-03-08T21:57:11.674734-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

TOML, o Tom's Obvious, Minimal Language (El Lenguaje Mínimo y Obvio de Tom), es un formato de archivo de configuración que es fácil de leer debido a su clara semántica. Los programadores lo utilizan para configurar aplicaciones de software porque es simple de analizar y genera mínima confusión o errores.

## Cómo hacerlo:

Dart no incluye soporte incorporado para TOML, pero puedes trabajar con archivos TOML utilizando paquetes de terceros como `toml`. Primero, agrega `toml` a tu `pubspec.yaml`:

```yaml
dependencies:
  toml: ^0.10.0
```

### Leyendo TOML

Para leer un archivo TOML, supongamos que tienes un archivo de configuración simple `config.toml`:

```toml
[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true
```

Puedes analizar este archivo TOML en Dart de la siguiente manera:

```dart
import 'dart:io';
import 'package:toml/toml.dart';

void main() async {
  var content = await File('config.toml').readAsString();
  var doc = TomlDocument.parse(content);
  var data = doc.toMap();

  print(data['database']); // Imprime la sección 'database'
}
```

Esto imprime:

```dart
{server: 192.168.1.1, ports: [8001, 8001, 8002], connection_max: 5000, enabled: true}
```

### Escribiendo TOML

Para crear contenido TOML, usa el `TomlBuilder` proporcionado por el paquete `toml`:

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

Esto generará e imprimirá una representación en cadena del contenido TOML, muy similar a nuestro archivo `config.toml`:

```toml
[database]
server = "192.168.1.1"
ports = [8001, 8001, 8002]
connection_max = 5000
enabled = true
```

Estos ejemplos muestran cómo leer y escribir archivos TOML, facilitando el trabajo con datos de configuración en tus aplicaciones Dart.
