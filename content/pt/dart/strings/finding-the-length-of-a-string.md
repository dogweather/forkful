---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:38.447779-07:00
description: "Encontrar o comprimento de uma String em Dart \xE9 sobre determinar\
  \ o n\xFAmero de unidades de c\xF3digo (essencialmente, o n\xFAmero de caracteres\
  \ se pensado de\u2026"
lastmod: '2024-03-09T21:06:10.620716-07:00'
model: gpt-4-0125-preview
summary: "Encontrar o comprimento de uma String em Dart \xE9 sobre determinar o n\xFA\
  mero de unidades de c\xF3digo (essencialmente, o n\xFAmero de caracteres se pensado\
  \ de\u2026"
title: Encontrando o comprimento de uma string
---

{{< edit_this_page >}}

## O Que e Por Quê?
Encontrar o comprimento de uma String em Dart é sobre determinar o número de unidades de código (essencialmente, o número de caracteres se pensado de forma simplista) em uma dada String. Programadores fazem isso para manipular strings de maneira mais precisa, como validar entradas, truncar textos de exibição, ou processar formatos de dados onde o comprimento importa (por exemplo, protocolos com mensagens prefixadas por comprimento).

## Como fazer:
Dart torna simples obter o comprimento de uma string usando a propriedade `length`. Aqui está um exemplo básico:

```dart
void main() {
  String myString = "Olá, Dart!";
  print("O comprimento de '\(myString)' é: \(myString.length)");
  // Saída: O comprimento de 'Olá, Dart!' é: 10
}
```
Esta propriedade conta o número de unidades de código UTF-16 na string, o que corresponde ao comprimento da string para a maioria dos casos de uso comuns.

Para um processamento de texto mais matizado, especialmente envolvendo caracteres Unicode fora do Plano Multilíngue Básico (BMP), considere usar o pacote `characters` para contar aglomerados de grafemas, que representa mais precisamente os caracteres percebidos pelo usuário.

Primeiro, adicione `characters` ao seu `pubspec.yaml`:

```yaml
dependencies:
  characters: ^1.2.0
```

Depois, use-o assim:

```dart
import 'package:characters/characters.dart';

void main() {
  String myEmojiString = "👨‍👩‍👧‍👦 família";
  print("O comprimento de '\(myEmojiString)' é: \(myEmojiString.characters.length)");
  // Saída: O comprimento de '👨‍👩‍👧‍👦 família' é: 8
}
```

Nesse exemplo, `myEmojiString.characters.length` nos dá o comprimento em termos de aglomerados de grafemas Unicode, o que é uma representação mais precisa para strings contendo caracteres complexos, como emojis ou marcas de caracteres combinados.
