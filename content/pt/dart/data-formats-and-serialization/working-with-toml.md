---
title:                "Trabalhando com TOML"
date:                  2024-03-08T21:57:33.574673-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## O que & Por quê?

**TOML**, ou *Tom's Obvious, Minimal Language* (Linguagem Mínima e Óbvia do Tom), é um formato de arquivo de configuração que é fácil de ler devido à sua semântica clara. Programadores o utilizam para configurar aplicações de software porque é direto para analisar e gera mínima confusão ou erros.

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
