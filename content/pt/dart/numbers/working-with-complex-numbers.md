---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:59.221328-07:00
description: "N\xFAmeros complexos, consistindo de uma parte real e uma imagin\xE1\
  ria (normalmente denotados como a + bi), estendem o conceito dos n\xFAmeros sem\
  \ dimens\xE3o para\u2026"
lastmod: '2024-03-13T22:44:46.274871-06:00'
model: gpt-4-0125-preview
summary: "N\xFAmeros complexos, consistindo de uma parte real e uma imagin\xE1ria\
  \ (normalmente denotados como a + bi), estendem o conceito dos n\xFAmeros sem dimens\xE3\
  o para um espa\xE7o bidimensional."
title: "Trabalhando com N\xFAmeros Complexos"
weight: 14
---

## O que & Por quê?

Números complexos, consistindo de uma parte real e uma imaginária (normalmente denotados como a + bi), estendem o conceito dos números sem dimensão para um espaço bidimensional. Programadores trabalham com números complexos em áreas como engenharia elétrica, computação quântica e dinâmica de fluidos para modelar fenômenos que não podem ser representados apenas em uma única dimensão de números reais.

## Como fazer:

O Dart por si só não inclui uma biblioteca interna para números complexos, necessitando ou da implementação de uma classe de número complexo personalizada ou do uso de uma biblioteca de terceiros. Uma escolha popular para tarefas de computação científica, que inclui suporte para números complexos, é o `package:scidart`.

### Implementando uma Classe Básica de Números Complexos

Para operações simples, você pode facilmente definir sua própria classe de número complexo:

```dart
class Complex {
  final double real;
  final double imaginary;

  Complex(this.real, this.imaginary);

  // Adição de dois números complexos
  Complex operator +(Complex other) {
    return Complex(real + other.real, imaginary + other.imaginary);
  }

  // Representação em string para fácil depuração
  @override
  String toString() => '${real} + ${imaginary}i';
}

void main() {
  var number1 = Complex(3, 4);
  var number2 = Complex(1, 2);

  var soma = number1 + number2;
  print(soma);  // 4.0 + 6.0i
}
```

### Usando o SciDart para Operações Avançadas

Para operações mais complexas ou quando o desempenho é crítico, o `package:scidart` oferece suporte abrangente para números complexos, entre outras funcionalidades de computação científica. Primeiro, adicione o SciDart ao seu pubspec.yaml:

```yaml
dependencies:
  scidart: ^0.0.1-dev.9
```

Veja como realizar operações básicas com números complexos usando o SciDart:

```dart
import 'package:scidart/numdart.dart';

void main() {
  // Criando números complexos
  var complexNum1 = Complex(real: 5, imaginary: 3);
  var complexNum2 = Complex(real: 2, imaginary: 7);

  // Adição
  var soma = complexAdd(complexNum1, complexNum2);
  
  // Multiplicação
  var produto = complexMultiply(complexNum1, complexNum2);

  print('Soma: ${soma.toString()}');  // Soma: Complex(real: 7.0, imaginary: 10.0)
  print('Produto: ${produto.toString()}');  // Produto: Complex(real: -11.0, imaginary: 41.0)
}
```

Estes exemplos demonstram a manipulação básica e a utilização de números complexos em Dart, tanto através da implementação personalizada quanto via a biblioteca SciDart, destacando a flexibilidade e o poder do Dart para tarefas de computação científica.
