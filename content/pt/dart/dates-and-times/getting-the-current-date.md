---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:02.923871-07:00
description: "Obter a data atual em Dart envolve consultar o sistema para saber a\
  \ data e a hora atuais. Esta funcionalidade \xE9 comumente usada em aplica\xE7\xF5\
  es para\u2026"
lastmod: '2024-03-13T22:44:46.292944-06:00'
model: gpt-4-0125-preview
summary: "Obter a data atual em Dart envolve consultar o sistema para saber a data\
  \ e a hora atuais. Esta funcionalidade \xE9 comumente usada em aplica\xE7\xF5es\
  \ para\u2026"
title: Obtendo a data atual
---

{{< edit_this_page >}}

## O que & Por quê?
Obter a data atual em Dart envolve consultar o sistema para saber a data e a hora atuais. Esta funcionalidade é comumente usada em aplicações para recursos como marcar eventos com timestamp, mostrar a data atual para usuários ou calcular durações. Saber como recuperar e manipular a data atual de forma eficiente é fundamental para agendamento, registros e recursos sensíveis ao tempo.

## Como fazer:
A biblioteca central do Dart oferece acesso direto à data e hora atuais por meio da classe `DateTime`. Aqui está o exemplo básico para obter a data atual:

```dart
void main() {
  DateTime now = DateTime.now();
  print(now); // Exemplo de saída: 2023-04-12 10:00:00.000
}
```

Se você precisar apenas da parte da data (ano, mês, dia), pode formatar o objeto `DateTime`:

```dart
void main() {
  DateTime now = DateTime.now();
  String formattedDate = "${now.year}-${now.month}-${now.day}";
  print(formattedDate); // Exemplo de saída: 2023-04-12
}
```

O Dart não inclui uma biblioteca integrada para formatação de datas mais complexa, mas você pode usar o pacote `intl` para esse propósito. Primeiro, adicione o pacote ao seu `pubspec.yaml`:

```yaml
dependencies:
  intl: ^0.17.0
```

Então, você pode formatar datas facilmente:

```dart
import 'package:intl/intl.dart';

void main() {
  DateTime now = DateTime.now();
  String formattedDate = DateFormat('yyyy-MM-dd').format(now);
  print(formattedDate); // Exemplo de saída: 2023-04-12
}
```

Para opções de formatação mais avançadas, explore a classe `DateFormat` fornecida pelo pacote `intl`, que suporta uma ampla gama de padrões e locais.
