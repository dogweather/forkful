---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:53:42.724433-07:00
description: "Como fazer: Em Dart, voc\xEA pode comparar datas usando a classe `DateTime`,\
  \ que oferece m\xE9todos como `isBefore`, `isAfter` e `isAtSameMomentAs` para\u2026"
lastmod: '2024-03-13T22:44:46.295129-06:00'
model: gpt-4-0125-preview
summary: "Em Dart, voc\xEA pode comparar datas usando a classe `DateTime`, que oferece\
  \ m\xE9todos como `isBefore`, `isAfter` e `isAtSameMomentAs` para compara\xE7\xE3\
  o direta."
title: Comparando duas datas
weight: 27
---

## Como fazer:
Em Dart, você pode comparar datas usando a classe `DateTime`, que oferece métodos como `isBefore`, `isAfter` e `isAtSameMomentAs` para comparação direta. Adicionalmente, a diferença entre datas pode ser determinada usando o método `difference()`, fornecendo um objeto `Duration` que detalha o intervalo entre os dois pontos no tempo.

Aqui está um exemplo básico ilustrando esses conceitos:

```dart
void main() {
  DateTime inicioDoEvento = DateTime(2023, 5, 15);
  DateTime fimDoEvento = DateTime(2023, 5, 20);
  
  // Verificando se uma data é anterior a outra
  if (inicioDoEvento.isBefore(fimDoEvento)) {
    print("A data de início do evento é anterior à data de término do evento.");
  }

  // Verificando se duas datas são as mesmas
  if (!inicioDoEvento.isAtSameMomentAs(fimDoEvento)) {
    print("As datas de início e término não são as mesmas.");
  }
  
  // Calculando a diferença entre duas datas
  Duration duracaoDoEvento = fimDoEvento.difference(inicioDoEvento);
  print("O evento dura ${duracaoDoEvento.inDays} dias.");
}

/*
Saída:
A data de início do evento é anterior à data de término do evento.
As datas de início e término não são as mesmas.
O evento dura 5 dias.
*/
```

Para manipulações de datas mais avançadas, como conversões de formato, você pode achar útil a classe `DateFormat` do pacote `intl`. Abaixo está um exemplo demonstrando como usá-la para formatar e comparar datas:

Primeiro, inclua o pacote `intl` no seu `pubspec.yaml`:

```yaml
dependencies:
  intl: ^0.17.0
```

Então, use-o da seguinte forma:

```dart
import 'package:intl/intl.dart';

void main() {
  DateTime dataDePartida = DateTime(2023, 5, 15);
  DateTime dataDeRetorno = DateTime.parse('2023-05-20');

  // Formatando datas
  var formatador = DateFormat('yyyy-MM-dd');
  print("Partida: ${formatador.format(dataDePartida)}");
  print("Retorno: ${formatador.format(dataDeRetorno)}");

  // Comparar usando strings formatadas
  if (formatador.format(dataDePartida) == formatador.format(dataDeRetorno)) {
    print("As datas de partida e retorno são as mesmas.");
  } else {
    print("As datas de partida e retorno são diferentes.");
  }
}

/*
Saída:
Partida: 2023-05-15
Retorno: 2023-05-20
As datas de partida e retorno são diferentes.
*/
```

Este exemplo mostra como comparar dois objetos `DateTime` diretamente e usando strings formatadas para comparações que precisam ignorar componentes específicos como o tempo.
