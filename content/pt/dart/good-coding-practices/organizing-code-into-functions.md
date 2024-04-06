---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:58.995631-07:00
description: "Como Fazer: No Dart, voc\xEA define uma fun\xE7\xE3o usando a palavra-chave\
  \ `void` se ela n\xE3o retornar um valor, ou especifica o tipo de valor que retorna\
  \ de outra\u2026"
lastmod: '2024-03-13T22:44:46.287624-06:00'
model: gpt-4-0125-preview
summary: "No Dart, voc\xEA define uma fun\xE7\xE3o usando a palavra-chave `void` se\
  \ ela n\xE3o retornar um valor, ou especifica o tipo de valor que retorna de outra\
  \ forma."
title: "Organizando c\xF3digo em fun\xE7\xF5es"
weight: 18
---

## Como Fazer:


### Função Básica
No Dart, você define uma função usando a palavra-chave `void` se ela não retornar um valor, ou especifica o tipo de valor que retorna de outra forma. Aqui está uma função simples que imprime uma mensagem de saudação:

```dart
void greet(String name) {
  print('Olá, $name!');
}

void main() {
  greet('Alice');  // Saída: Olá, Alice!
}
```

### Retornando um Valor
Funções podem retornar valores. O exemplo a seguir recebe dois inteiros como entrada e retorna a soma deles:

```dart
int add(int a, int b) {
  return a + b;
}

void main() {
  var soma = add(5, 3);
  print(soma);  // Saída: 8
}
```

### Funções Anônimas
O Dart suporta funções anônimas (também conhecidas como expressões lambda ou closures), que podem ser úteis para funcionalidades curtas e imediatas. Veja como usar uma função anônima com o método `forEach` de uma lista:

```dart
void main() {
  var frutas = ['maçã', 'banana', 'cereja'];
  frutas.forEach((item) {
    print(item);
  });
  // Saída:
  // maçã
  // banana
  // cereja
}
```

### Sintaxe de Seta para Funções de Única Expressão
Para funções que contêm apenas uma única expressão, o Dart oferece uma sintaxe concisa usando a notação "seta" (`=>`). Isso é especialmente útil para funções curtas ou ao passar funções como argumentos:

```dart
int quadrado(int num) => num * num;

void main() {
  print(quadrado(4));  // Saída: 16
}
```

### Usando Bibliotecas de Terceiros
Para funcionalidades mais complexas ou especializadas, os programadores de Dart frequentemente dependem de bibliotecas de terceiros. Considere a biblioteca `http` para fazer solicitações HTTP. Primeiro, adicione `http` ao seu arquivo pubspec.yaml em dependências:

```
dependencies:
  http: ^0.13.3
```

Então, você pode usá-la para buscar dados da web:

```dart
import 'package:http/http.dart' as http;

Future<void> fetchUserData() async {
  var response = await http.get(Uri.parse('https://api.example.com/users/1'));
  print(response.body);
}

void main() {
  fetchUserData();
  // Saída esperada: Dados JSON do usuário. A saída real dependerá da resposta da API.
}
```

Lembre-se, ao organizar seu código Dart em funções, pense em reutilização, clareza e no princípio da responsabilidade única. Isso não apenas torna o seu código mais limpo, mas também mais fácil para outros (e para você no futuro) entenderem e manterem.
