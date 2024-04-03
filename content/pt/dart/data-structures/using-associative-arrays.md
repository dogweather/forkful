---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:51.218382-07:00
description: "Como: Dart oferece uma sintaxe simples para criar e manipular Maps.\
  \ Abaixo est\xE3o exemplos demonstrando opera\xE7\xF5es b\xE1sicas como cria\xE7\
  \xE3o, adi\xE7\xE3o de elementos\u2026"
lastmod: '2024-03-13T22:44:46.273809-06:00'
model: gpt-4-0125-preview
summary: Dart oferece uma sintaxe simples para criar e manipular Maps.
title: Usando arrays associativos
weight: 15
---

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
