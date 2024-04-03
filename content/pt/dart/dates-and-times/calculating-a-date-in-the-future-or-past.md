---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:53:41.057593-07:00
description: "Como fazer: Dart oferece suporte robusto para manipula\xE7\xE3o de datas\
  \ por meio de sua classe `DateTime`. Veja como voc\xEA pode calcular datas futuras\
  \ ou\u2026"
lastmod: '2024-03-13T22:44:46.296159-06:00'
model: gpt-4-0125-preview
summary: "Dart oferece suporte robusto para manipula\xE7\xE3o de datas por meio de\
  \ sua classe `DateTime`."
title: Calculando uma data no futuro ou passado
weight: 26
---

## Como fazer:
Dart oferece suporte robusto para manipulação de datas por meio de sua classe `DateTime`. Veja como você pode calcular datas futuras ou passadas usando Dart nativo, sem precisar de bibliotecas de terceiros.

### Calculando uma Data Futura
Para calcular uma data no futuro, você cria um objeto `DateTime` e usa o método `add` com a duração desejada.

```dart
DateTime hoje = DateTime.now();
Duration dezDias = Duration(days: 10);
DateTime dataFutura = hoje.add(dezDias);

print(dataFutura); // Saída: 2023-04-21 14:22:35.123456 (saída de exemplo, depende da data e hora atual)
```

### Calculando uma Data Passada
Para calcular uma data no passado, você usa o método `subtract` em um objeto `DateTime` com a duração necessária.

```dart
DateTime hoje = DateTime.now();
Duration quinzeDiasAtras = Duration(days: 15);
DateTime dataPassada = hoje.subtract(quinzeDiasAtras);

print(dataPassada); // Saída: 2023-03-27 14:22:35.123456 (saída de exemplo, depende da data e hora atual)
```

### Usando Bibliotecas de Terceiros
Embora as capacidades nativas de Dart para manipulação de datas sejam poderosas, você pode se encontrar precisando de operações mais específicas, como analisar ou formatar datas mais facilmente, ou realizar cálculos complexos. Nestes casos, o pacote `time` pode ser muito útil.

Primeiro, adicione `time` às suas dependências no `pubspec.yaml`:

```yaml
dependencies:
  time: ^2.0.0
```

Então, você pode usá-lo para realizar cálculos semelhantes com maior legibilidade:

```dart
import 'package:time/time.dart';

void main() {
  DateTime hoje = DateTime.now();

  // Calculando uma data futura
  DateTime dataFutura = hoje + 10.days;
  print(dataFutura); // Formato de saída: 2023-04-21 14:22:35.123456

  // Calculando uma data passada
  DateTime dataPassada = hoje - 15.days;
  print(dataPassada); // Formato de saída: 2023-03-27 14:22:35.123456
}
```

Estes exemplos ilustram manipulações básicas de datas em Dart, incluindo adicionar e subtrair tempo de ou para uma data atual, demonstrando o quão fácil pode ser gerenciar datas em aplicações Dart.
