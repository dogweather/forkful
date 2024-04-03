---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:15.990853-07:00
description: "Como Fazer: A biblioteca principal do Dart simplifica a an\xE1lise de\
  \ datas atrav\xE9s da classe `DateTime`. Para casos diretos em que voc\xEA conhece\
  \ o formato da\u2026"
lastmod: '2024-03-13T22:44:46.291871-06:00'
model: gpt-4-0125-preview
summary: "A biblioteca principal do Dart simplifica a an\xE1lise de datas atrav\xE9\
  s da classe `DateTime`."
title: Analisando uma data a partir de uma string
weight: 30
---

## Como Fazer:
A biblioteca principal do Dart simplifica a análise de datas através da classe `DateTime`. Para casos diretos em que você conhece o formato da string de data, você pode usar o método `DateTime.parse()`. No entanto, para cenários mais complexos ou ao lidar com múltiplos formatos, o pacote `intl`, especificamente a classe `DateFormat`, torna-se inestimável.

### Usando a Biblioteca Principal do Dart:
```dart
void main() {
  // Usando DateTime.parse()
  var dateString = "2023-10-31";
  var parsedDate = DateTime.parse(dateString);
  
  print(parsedDate); // 2023-10-31 00:00:00.000
}
```

### Usando o Pacote `intl`:
Primeiramente, adicione o pacote `intl` ao seu arquivo `pubspec.yaml`:
```yaml
dependencies:
  intl: ^0.17.0
```
Em seguida, importe o pacote e use `DateFormat` para análise:
```dart
import 'package:intl/intl.dart';

void main() {
  var dateString = "October 31, 2023";
  var dateFormat = DateFormat("MMMM dd, yyyy");
  var parsedDate = dateFormat.parse(dateString);
  
  print(parsedDate); // 2023-10-31 00:00:00.000
}
```
O pacote `intl` oferece opções robustas para análise de datas, permitindo o manuseio de vários formatos internacionais de datas sem problemas.
