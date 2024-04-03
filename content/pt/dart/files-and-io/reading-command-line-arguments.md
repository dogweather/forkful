---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:40.654561-07:00
description: "Ler argumentos de linha de comando em Dart permite que programadores\
  \ insiram dados diretamente no console ao executar um programa Dart, aumentando\
  \ sua\u2026"
lastmod: '2024-03-13T22:44:46.298285-06:00'
model: gpt-4-0125-preview
summary: "Ler argumentos de linha de comando em Dart permite que programadores insiram\
  \ dados diretamente no console ao executar um programa Dart, aumentando sua interatividade\
  \ e flexibilidade para v\xE1rios casos de uso, incluindo scripts de automa\xE7\xE3\
  o, ferramentas CLI ou processamento em lote."
title: Lendo argumentos de linha de comando
weight: 23
---

## O Que & Por Que?

Ler argumentos de linha de comando em Dart permite que programadores insiram dados diretamente no console ao executar um programa Dart, aumentando sua interatividade e flexibilidade para vários casos de uso, incluindo scripts de automação, ferramentas CLI ou processamento em lote. Essa característica é fundamental para criar aplicativos de linha de comando adaptáveis e amigáveis ao usuário.

## Como fazer:

Dart oferece uma abordagem direta para acessar argumentos de linha de comando via a `List<String> args` no método main. Abaixo está um exemplo simples que demonstra como ler e utilizar argumentos de linha de comando.

```dart
// main.dart
void main(List<String> args) {
  print('Argumentos de Linha de Comando:');
  for (var i = 0; i < args.length; i++) {
    print('${i + 1}: ${args[i]}');
  }
}
```

Para executar este programa Dart e passar argumentos de linha de comando, use o Dart CLI assim:

```shell
dart run main.dart Olá Mundo!
```

Saída esperada:

```
Argumentos de Linha de Comando:
1: Olá
2: Mundo!
```

### Usando uma Biblioteca Terceirizada Popular: `args`

Embora as capacidades internas do Dart para lidar com argumentos de linha de comando sejam robustas para muitas aplicações, o pacote `args` oferece uma forma refinada de definir e analisar argumentos de linha de comando para necessidades mais complexas.

Primeiro, adicione o pacote `args` ao seu `pubspec.yaml`:

```yaml
dependencies:
  args: ^2.0.0
```

Em seguida, use-o em seu programa da seguinte forma:

```dart
// Usando o pacote 'args'
import 'package:args/args.dart';

void main(List<String> arguments) {
  final parser = ArgParser()..addOption('name', abbr: 'n');
  final argResults = parser.parse(arguments);

  if (argResults.wasParsed('name')) {
    print('Olá, ${argResults['name']}!');
  } else {
    print('Nenhum nome fornecido.');
  }
}
```

Execute o programa com um argumento nomeado:

```shell
dart run main.dart --name=João
```

Saída esperada:

```
Olá, João!
```

Esta introdução simples ao parseamento de argumentos de linha de comando, tanto nativamente quanto com a biblioteca `args`, mostra como o Dart pode tratar entradas de usuário diretamente do console, abrindo um caminho para criar aplicações CLI mais interativas e dinâmicas.
