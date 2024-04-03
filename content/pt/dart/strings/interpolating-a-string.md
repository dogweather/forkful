---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:48.075967-07:00
description: "Como Fazer: No Dart, a interpola\xE7\xE3o de strings \xE9 direta, utilizando\
  \ o s\xEDmbolo `$` para interpolar express\xF5es diretamente dentro de literais\
  \ de string."
lastmod: '2024-03-13T22:44:46.266375-06:00'
model: gpt-4-0125-preview
summary: "No Dart, a interpola\xE7\xE3o de strings \xE9 direta, utilizando o s\xED\
  mbolo `$` para interpolar express\xF5es diretamente dentro de literais de string."
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
