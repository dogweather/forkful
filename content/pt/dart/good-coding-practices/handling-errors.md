---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:12.478008-07:00
description: "O tratamento de erros em Dart consiste em antecipar e gerir exce\xE7\
  \xF5es que surgem durante a execu\xE7\xE3o do programa para aumentar a confiabilidade\
  \ e\u2026"
lastmod: '2024-03-13T22:44:46.289763-06:00'
model: gpt-4-0125-preview
summary: "O tratamento de erros em Dart consiste em antecipar e gerir exce\xE7\xF5\
  es que surgem durante a execu\xE7\xE3o do programa para aumentar a confiabilidade\
  \ e usabilidade."
title: Gerenciando erros
weight: 16
---

## Como Fazer:
O Dart suporta dois tipos de erros: erros em *tempo de compilação* e erros em *tempo de execução*. Os erros de tempo de compilação são detectados pelo analisador Dart antes da execução do código, enquanto os erros de tempo de execução, ou exceções, ocorrem durante a execução. Veja como você pode tratar exceções em Dart:

### Try-Catch
Use `try-catch` para capturar exceções e prevenir que elas façam seu aplicativo falhar:

```dart
try {
  var result = 100 ~/ 0; // Tentativa de divisão por zero, lança uma exceção
} catch (e) {
  print('Exceção capturada: $e'); // Trata a exceção
}
```
Saída de exemplo: `Exceção capturada: IntegerDivisionByZeroException`

### Exceção Específica
Para tratar exceções específicas, mencione a exceção após `catch`:

```dart
try {
  var result = 100 ~/ 0;
} on IntegerDivisionByZeroException {
  print('Não é possível dividir por zero.'); // Trata especificamente exceções de divisão por zero
}
```
Saída de exemplo: `Não é possível dividir por zero.`

### Rastreio de Pilha
Para obter um rastreio de pilha para depuração, use um segundo parâmetro no bloco catch:

```dart
try {
  var result = 100 ~/ 0;
} catch (e, s) {
  print('Exceção: $e');
  print('Rastreio de pilha: $s'); // Imprime rastreio de pilha para depuração
}
```

### Finally
Use `finally` para executar código após try/catch, independentemente de uma exceção ter sido lançada ou não:

```dart
try {
  var result = 100 ~/ 0;
} catch (e) {
  print('Exceção capturada: $e');
} finally {
  print('Isso é sempre executado.'); // Código de limpeza ou passos finais
}
```
Saída de exemplo:
```
Exceção capturada: IntegerDivisionByZeroException
Isso é sempre executado.
```

### Bibliotecas de Terceiros
Embora a biblioteca central do Dart seja robusta para o tratamento de erros, você também pode usar pacotes de terceiros como `dartz` para programação funcional que introduz conceitos como `Either` e `Option` que podem ser usados para tratamento de erros. Aqui está um exemplo usando `dartz` para tratamento de erros:

1. Adicione `dartz` ao seu arquivo `pubspec.yaml` sob dependências:
```yaml
dependencies:
  dartz: ^0.10.0
```

2. Use `Either` para tratar erros de forma graciosa em seu código Dart:
```dart
import 'package:dartz/dartz.dart';

Either<String, int> divide(int dividendo, int divisor) {
  if (divisor == 0) {
    return Left('Não é possível dividir por zero.');
  } else {
    return Right(dividendo ~/ divisor);
  }
}

void main() {
  final result = divide(100, 0);
  result.fold(
    (left) => print('Erro: $left'), 
    (right) => print('Resultado: $right')
  );
}
```
Saída de exemplo: `Erro: Não é possível dividir por zero.`

A parte `Left` geralmente representa o erro, e a parte `Right` representa o sucesso. Esse padrão permite tratar erros de uma maneira mais funcional, oferecendo clareza e controle sobre a gestão de erros.
