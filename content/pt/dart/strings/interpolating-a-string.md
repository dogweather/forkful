---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:48.075967-07:00
description: "A interpola\xE7\xE3o de strings \xE9 o processo de injetar valores de\
  \ vari\xE1veis diretamente em strings, muitas vezes para criar mensagens significativas\
  \ sem\u2026"
lastmod: '2024-03-13T22:44:46.266375-06:00'
model: gpt-4-0125-preview
summary: "A interpola\xE7\xE3o de strings \xE9 o processo de injetar valores de vari\xE1\
  veis diretamente em strings, muitas vezes para criar mensagens significativas sem\
  \ concatena\xE7\xF5es complicadas."
title: Interpolando uma string
weight: 8
---

## Como Fazer:
No Dart, a interpolação de strings é direta, utilizando o símbolo `$` para interpolar expressões diretamente dentro de literais de string:

```dart
void main() {
  String nome = 'Dart';
  int ano = 2023;
  // Interpolação simples de variável
  print('Aprendendo $nome em $ano!');
  // Saída: Aprendendo Dart em 2023!
  
  // Interpolando expressões
  print('Em dois anos, será ${ano + 2}.');
  // Saída: Em dois anos, será 2025.
}
```

No caso em que você tem expressões mais complexas ou quer realizar operações dentro da própria string, encerre a expressão em `${}`. Dart não possui bibliotecas de terceiros populares especificamente para interpolação de strings já que está bem equipado nativamente para lidar com cenários variados e complexos.
