---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:40.558927-07:00
description: "Um shell interativo (REPL - Read-Evaluate-Print Loop) para Dart permite\
  \ que programadores digitem e executem c\xF3digo Dart linha por linha sem a necessidade\u2026"
lastmod: '2024-03-09T21:06:10.630531-07:00'
model: gpt-4-0125-preview
summary: "Um shell interativo (REPL - Read-Evaluate-Print Loop) para Dart permite\
  \ que programadores digitem e executem c\xF3digo Dart linha por linha sem a necessidade\u2026"
title: Usando um shell interativo (REPL)
---

{{< edit_this_page >}}

## O Que & Porquê?

Um shell interativo (REPL - Read-Evaluate-Print Loop) para Dart permite que programadores digitem e executem código Dart linha por linha sem a necessidade de compilar scripts inteiros. Esta ferramenta é inestimável para aprender a sintaxe do Dart, experimentar trechos de código ou depurar oferecendo feedback instantâneo e facilitando o teste iterativo.

## Como:

O Dart não vem com um REPL integrado. No entanto, você pode alcançar uma funcionalidade semelhante a um REPL usando o DartPad (online) ou utilizando ferramentas de terceiros como `dart_repl`.

**Usando o DartPad:**

O DartPad (https://dartpad.dev) é um editor de Dart online que permite escrever e executar código Dart no seu navegador web. Embora não seja um REPL tradicional de linha de comando, ele fornece uma experiência semelhante para experimentação rápida.

Simplesmente acesse o site, digite seu código Dart no painel esquerdo e clique em "Executar" para ver a saída no painel direito.

Exemplo:
```dart
void main() {
  print('Hello, Dart!');
}
```
Saída:
```
Hello, Dart!
```

**Usando `dart_repl` (ferramenta de terceiros):**

Primeiramente, instale `dart_repl` via pub globalmente:

```shell
dart pub global activate dart_repl
```

Em seguida, execute `dart_repl` do seu terminal:

```shell
dart_repl
```

Agora, você pode começar a digitar declarações Dart diretamente no shell. Por exemplo:

```dart
>>> print('Hello, REPL!');
Hello, REPL!
>>> int add(int x, int y) => x + y;
>>> print(add(5, 7));
12
```

Esses métodos fornecem um caminho rápido para experimentar código Dart de forma instantânea, facilitando significativamente a curva de aprendizado e melhorando a produtividade.
