---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:51.218382-07:00
description: "Arrays associativos em Dart, conhecidos como Maps, s\xE3o estruturas\
  \ de dados que armazenam dados em pares de chave-valor. Eles permitem que os programadores\u2026"
lastmod: '2024-03-11T00:14:19.956065-06:00'
model: gpt-4-0125-preview
summary: "Arrays associativos em Dart, conhecidos como Maps, s\xE3o estruturas de\
  \ dados que armazenam dados em pares de chave-valor. Eles permitem que os programadores\u2026"
title: Usando arrays associativos
---

{{< edit_this_page >}}

## O Quê & Por Quê?

Arrays associativos em Dart, conhecidos como Maps, são estruturas de dados que armazenam dados em pares de chave-valor. Eles permitem que os programadores acessem elementos não através de índices, mas sim de chaves, tornando a recuperação de dados intuitiva e eficiente, especialmente ao trabalhar com dados estruturados onde cada elemento tem um identificador único.

## Como:

Dart oferece uma sintaxe simples para criar e manipular Maps. Abaixo estão exemplos demonstrando operações básicas como criação, adição de elementos e busca de valores.

```dart
void main() {
  // Criando um map
  var fruitColors = {
    'apple': 'red',
    'banana': 'yellow',
    'grape': 'purple'
  };

  // Adicionando um novo par chave-valor
  fruitColors['orange'] = 'orange';

  // Acessando um valor pela sua chave
  print(fruitColors['apple']); // Saída: red

  // Atualizando um valor
  fruitColors['banana'] = 'green';

  // Iterando sobre o Map
  fruitColors.forEach((fruit, color) {
    print('$fruit: $color');
  });
  // Saída Exemplar:
  // apple: red
  // banana: green
  // grape: purple
  // orange: orange
}
```

Para estruturas de dados complexas ou funcionalidades estendidas, programadores Dart frequentemente contam com bibliotecas adicionais. Uma dessas bibliotecas é `collection`, que fornece tipos de coleções avançados e utilidades. Embora `collection` não modifique a forma básica como os Maps são manipulados, ela os enriquece com funções de utilidade e tipos de coleção mais sofisticados. Veja como você poderia usá-la para uma tarefa mais específica, como ordenar um Map pelos seus valores:

Primeiro, assegure que o pacote `collection` está incluído no seu arquivo `pubspec.yaml`:

```yaml
dependencies:
  collection: ^1.15.0
```

Então, você pode usá-lo da seguinte forma:

```dart
import 'package:collection/collection.dart';

void main() {
  var fruitColors = {
    'apple': 'red',
    'banana': 'yellow',
    'grape': 'purple',
    'orange': 'orange'
  };

  // Ordenando o Map pelos seus valores (cores)
  var sortedFruitsByColor = SplayTreeMap.from(
    fruitColors,
    (key1, key2) => fruitColors[key1]!.compareTo(fruitColors[key2]!)
  );

  print(sortedFruitsByColor);
  // Saída:
  // {orange: orange, apple: red, banana: yellow, grape: purple}
}
```

Este exemplo demonstra a ordenação das entradas de um Map baseada em seus valores, mostrando como Dart e seu ecossistema vibrante podem manejar de forma ágil arrays associativos para manipulação de dados mais sofisticada.
