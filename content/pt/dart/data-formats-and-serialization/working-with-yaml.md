---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:28.535190-07:00
description: "Como fazer: Em Dart, trabalhar com YAML geralmente envolve o uso de\
  \ uma biblioteca de terceiros, j\xE1 que a linguagem n\xE3o inclui recursos de an\xE1\
  lise de YAML\u2026"
lastmod: '2024-03-13T22:44:46.303551-06:00'
model: gpt-4-0125-preview
summary: "Em Dart, trabalhar com YAML geralmente envolve o uso de uma biblioteca de\
  \ terceiros, j\xE1 que a linguagem n\xE3o inclui recursos de an\xE1lise de YAML\
  \ integrados."
title: Trabalhando com YAML
weight: 41
---

## Como fazer:
Em Dart, trabalhar com YAML geralmente envolve o uso de uma biblioteca de terceiros, já que a linguagem não inclui recursos de análise de YAML integrados. Uma escolha popular é o pacote `yaml`. Para começar, você precisará adicionar este pacote ao seu `pubspec.yaml`:

```yaml
dependencies:
  yaml: ^3.1.0
```

Lembre-se de executar `pub get` para buscar o pacote.

### Lendo YAML
Para ler um arquivo YAML, primeiro, importe o pacote `yaml` e depois use a função `loadYaml`:

```dart
import 'package:yaml/yaml.dart';
import 'dart:io';

void main() {
  final file = File('config.yaml').readAsStringAsync();
  final yamlMap = loadYaml(file);

  print(yamlMap['name']); // Saída: John Doe
}

```

Assumindo que seu arquivo `config.yaml` se pareça com isso:

```yaml
name: John Doe
age: 30
```

### Escrevendo YAML
Embora o pacote `yaml` seja ótimo para análise, ele não suporta a escrita de YAML. Para isso, você pode precisar converter seus dados para YAML manualmente ou usar outro pacote, se disponível. Ou, de forma mais direta, gerenciar suas transformações de dados e saídas como strings que correspondem à sintaxe YAML:

```dart
Map<String, dynamic> data = {
  'name': 'Jane Doe',
  'age': 29,
};

String toYamlString(Map<String, dynamic> map) {
  String yaml = '';
  map.forEach((key, value) {
    yaml += '$key: $value\n';
  });
  return yaml;
}

void main() {
  print(toYamlString(data)); // Saída: name: Jane Doe
                             //         age: 29
}
```

Essa é uma abordagem rudimentar e pode não ser adequada para estruturas de dados complexas ou recursos especiais do YAML. Para necessidades sofisticadas, você pode precisar procurar ou contribuir com um pacote Dart mais abrangente.
