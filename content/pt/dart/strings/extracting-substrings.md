---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:00.194098-07:00
description: "Extrair substrings trata-se de recuperar por\xE7\xF5es espec\xEDficas\
  \ de uma string baseadas em suas posi\xE7\xF5es ou padr\xF5es. Programadores fazem\
  \ isso para tarefas\u2026"
lastmod: '2024-03-13T22:44:46.269615-06:00'
model: gpt-4-0125-preview
summary: "Extrair substrings trata-se de recuperar por\xE7\xF5es espec\xEDficas de\
  \ uma string baseadas em suas posi\xE7\xF5es ou padr\xF5es. Programadores fazem\
  \ isso para tarefas\u2026"
title: Extraindo substrings
weight: 6
---

## O Que & Por Que?
Extrair substrings trata-se de recuperar porções específicas de uma string baseadas em suas posições ou padrões. Programadores fazem isso para tarefas como a análise de entradas de usuários, manipulação de dados, ou extração de informações relevantes de fontes de texto maiores.

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
