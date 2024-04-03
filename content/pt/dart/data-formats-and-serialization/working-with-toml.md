---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:33.574673-07:00
description: "Como fazer: **Dart** n\xE3o inclui suporte embutido para TOML, mas voc\xEA\
  \ pode trabalhar com arquivos TOML usando pacotes de terceiros como `toml`. Primeiro,\u2026"
lastmod: '2024-03-13T22:44:46.306932-06:00'
model: gpt-4-0125-preview
summary: "**Dart** n\xE3o inclui suporte embutido para TOML, mas voc\xEA pode trabalhar\
  \ com arquivos TOML usando pacotes de terceiros como `toml`."
title: Trabalhando com TOML
weight: 39
---

## Como fazer:
**Dart** não inclui suporte embutido para TOML, mas você pode trabalhar com arquivos TOML usando pacotes de terceiros como `toml`. Primeiro, adicione `toml` ao seu `pubspec.yaml`:

```yaml
dependencies:
  toml: ^0.10.0
```

### Lendo TOML
Para ler um arquivo TOML, vamos supor que você tenha um arquivo de configuração simples `config.toml`:

```toml
[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true
```

Você pode analisar este arquivo TOML em Dart da seguinte maneira:

```dart
import 'dart:io';
import 'package:toml/toml.dart';

void main() async {
  var content = await File('config.toml').readAsString();
  var doc = TomlDocument.parse(content);
  var data = doc.toMap();

  print(data['database']); // Imprimir a seção 'database'
}
```

Isso imprime:

```dart
{server: 192.168.1.1, ports: [8001, 8001, 8002], connection_max: 5000, enabled: true}
```

### Escrevendo em TOML
Para criar conteúdo em TOML, use o `TomlBuilder` fornecido pelo pacote `toml`:

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

Isso irá gerar e imprimir uma representação em string do conteúdo TOML, muito similar ao nosso arquivo `config.toml`:

```toml
[database]
server = "192.168.1.1"
ports = [8001, 8001, 8002]
connection_max = 5000
enabled = true
```

Estes exemplos mostram como ler e escrever em arquivos TOML, facilitando o trabalho com dados de configuração em suas aplicações Dart.
