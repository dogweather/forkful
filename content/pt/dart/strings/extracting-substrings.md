---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:00.194098-07:00
description: "Como Fazer: No Dart, voc\xEA pode usar diversos m\xE9todos para extrair\
  \ substrings, como `substring()`, `split()` e express\xF5es regulares. Cada m\xE9\
  todo serve a\u2026"
lastmod: '2024-03-13T22:44:46.269615-06:00'
model: gpt-4-0125-preview
summary: "No Dart, voc\xEA pode usar diversos m\xE9todos para extrair substrings,\
  \ como `substring()`, `split()` e express\xF5es regulares."
title: Extraindo substrings
weight: 6
---

## Como Fazer:
No Dart, você pode usar diversos métodos para extrair substrings, como `substring()`, `split()` e expressões regulares. Cada método serve a diferentes propósitos e oferece flexibilidade no manuseio de strings.

### Usando `substring()`:
O método `substring()` é direto. Você especifica o índice de início (e, opcionalmente, o de fim) para fatiar a string.

```dart
void main() {
  String exemplo = "Olá, Mundo!";
  String resultado = exemplo.substring(7, 12);
  print(resultado); // Saída: Mundo
}
```

### Usando `split()`:
Divida uma string em uma lista de substrings baseadas em um padrão (como um espaço ou vírgula), e então acesse a substring pelo índice.

```dart
void main() {
  String exemplo = "Dart é divertido";
  List<String> partes = exemplo.split(' ');
  String resultado = partes[1]; // Acesso por índice
  print(resultado); // Saída: é
}
```

### Usando Expressões Regulares:
Para padrões complexos, a classe `RegExp` do Dart é poderosa. Use-a para encontrar padrões e extrair substrings.

```dart
void main() {
  String exemplo = "Email: exemplo@mail.com";
  RegExp regExp = RegExp(r"\b\w+@\w+\.\w+\b");
  String email = regExp.stringMatch(exemplo)!;
  print(email); // Saída: exemplo@mail.com
}
```

### Bibliotecas de Terceiros:
Embora a biblioteca padrão do Dart seja bastante capaz, você pode encontrar cenários onde uma biblioteca de terceiros poderia simplificar sua tarefa. Uma escolha popular para manipulação de strings e correspondência de padrões não é especificamente defendida aqui, pois as capacidades integradas do Dart muitas vezes são suficientes. No entanto, sempre verifique o [pub.dev](https://pub.dev) para quaisquer bibliotecas que possam atender melhor às suas necessidades específicas.
