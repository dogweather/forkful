---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:11.561042-07:00
description: "Express\xF5es regulares (regex) em Dart oferecem uma maneira poderosa\
  \ de buscar e manipular strings, permitindo que programadores realizem tarefas complexas\u2026"
lastmod: '2024-03-13T22:44:46.270619-06:00'
model: gpt-4-0125-preview
summary: "Express\xF5es regulares (regex) em Dart oferecem uma maneira poderosa de\
  \ buscar e manipular strings, permitindo que programadores realizem tarefas complexas\
  \ de processamento de texto de forma eficiente."
title: "Usando express\xF5es regulares"
weight: 11
---

## Como fazer:
Dart utiliza a classe `RegExp` para expressões regulares. Aqui está um exemplo básico para combinar um padrão simples dentro de uma string:

```dart
void main() {
  var pattern = RegExp(r'\bDart\b');
  var text = 'Aprender programação Dart é empolgante.';

  if (pattern.hasMatch(text)) {
    print('Combinação encontrada!');
  } else {
    print('Nenhuma combinação encontrada.');
  }
  // Saída: Combinação encontrada!
}
```

Para extrair combinações de uma string, você pode usar o método `allMatches`. Esse método retorna um iterável de combinações:

```dart
void main() {
  var pattern = RegExp(r'\b\w+\b');
  var text = 'Dart é incrível!';

  var matches = pattern.allMatches(text);
  for (final match in matches) {
    print(match.group(0)); // Isso imprime as substrings correspondentes.
  }
  // Saída:
  // Dart
  // é
  // incrível
}
```

Substituir texto pode ser alcançado usando os métodos `replaceFirst` ou `replaceAll`:

```dart
void main() {
  var pattern = RegExp(r'\bDart\b');
  var text = 'Dart não é apenas um dardo.';
  
  // Substituir primeira ocorrência
  var modifiedText = text.replaceFirst(pattern, 'Flutter');
  print(modifiedText); 
  // Saída: Flutter não é apenas um dardo.

  // Substituir todas as ocorrências
  modifiedText = text.replaceAll(pattern, 'Flutter');
  print(modifiedText);
  // Saída: Flutter não é apenas um flutter.
}
```

Dividir uma string por um padrão regex é simples usando o método `split`:

```dart
void main() {
  var pattern = RegExp(r'\s+'); // Corresponde a qualquer caractere de espaço em branco
  var text = 'Dart é divertido';

  var parts = text.split(pattern);
  print(parts); 
  // Saída: [Dart, é, divertido]
}
```

Para análises ou validações complexas que não são suportadas diretamente pelo `RegExp` de Dart, você pode considerar bibliotecas de terceiros, mas a biblioteca padrão de Dart é frequentemente suficiente para tarefas comuns de regex, enfatizando sua utilidade e versatilidade no manuseio de expressões regulares.
