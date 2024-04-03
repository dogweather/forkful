---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:36.265343-07:00
description: "Como fazer: Em Dart, voc\xEA pode imprimir sa\xEDdas de depura\xE7\xE3\
  o usando a fun\xE7\xE3o `print()`. Veja como exibir mensagens simples e valores\
  \ de vari\xE1veis."
lastmod: '2024-03-13T22:44:46.284476-06:00'
model: gpt-4-0125-preview
summary: "Em Dart, voc\xEA pode imprimir sa\xEDdas de depura\xE7\xE3o usando a fun\xE7\
  \xE3o `print()`."
title: "Imprimindo a sa\xEDda de depura\xE7\xE3o"
weight: 33
---

## Como fazer:
Em Dart, você pode imprimir saídas de depuração usando a função `print()`. Veja como exibir mensagens simples e valores de variáveis:

```dart
void main() {
  String cumprimento = "Olá, Dart!";
  print(cumprimento); // Imprime: Olá, Dart!

  int numero = 42;
  print('O número é $numero.'); // Imprime: O número é 42.
}
```

Para dados estruturados, como listas ou objetos, o método `toString()` do Dart pode não fornecer detalhes suficientes. Nestes casos, você pode usar a função `jsonEncode` da biblioteca `dart:convert` do Dart para converter os dados em uma string JSON para uma saída mais legível:

```dart
import 'dart:convert';

void main() {
  var usuario = {
    'nome': 'John Doe',
    'idade': 30,
    'emails': ['john.doe@example.com', 'john@example.com'],
  };

  print(jsonEncode(usuario));
  // Imprime: {"nome":"John Doe","idade":30,"emails":["john.doe@example.com","john@example.com"]}
}
```

Quando capacidades de depuração mais sofisticadas são necessárias, como registro de log com diferentes níveis de importância (informação, advertência, erro), você pode usar bibliotecas de terceiros como `logger`. Veja como usá-lo:

1. Adicione `logger` ao seu `pubspec.yaml`:

```yaml
dependencies:
  logger: ^1.0.0
```

2. Use `logger` no seu código Dart:

```dart
import 'package:logger/logger.dart';

var logger = Logger();

void main() {
  logger.d("Esta é uma mensagem de depuração");
  logger.w("Esta é uma mensagem de advertência");
  logger.e("Esta é uma mensagem de erro");
}
```

A saída será mais informativa, mostrando o nível da mensagem e a própria mensagem, facilitando a distinção entre diferentes tipos de mensagens de log.
