---
title:                "Convertendo uma data em uma string"
date:                  2024-03-08T21:53:57.127309-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## O Que & Por Quê?

Converter uma data em uma string em Dart é uma tarefa comum quando você precisa exibir informações de data e hora em um formato legível por humanos, ou quando pretende serializar dados para armazenamento ou transmissão. Esse processo permite a fácil representação e manipulação de valores de data e hora em um formato que é ao mesmo tempo compreensível e pode ser personalizado dependendo do caso de uso.

## Como fazer:

Dart fornece a classe `DateTime` para lidar com datas e horas, e o pacote `intl` para formatação. Primeiramente, assegure-se de ter o pacote `intl` adicionando `intl: ^0.17.0` (ou a versão mais recente) ao seu arquivo `pubspec.yaml`.

### Usando a Biblioteca Central do Dart

```dart
DateTime now = DateTime.now();
String formattedDate = "${now.year}-${now.month}-${now.day}";
print(formattedDate); // Saída: 2023-4-12 (por exemplo, isso depende da data atual)
```

Este exemplo constrói diretamente uma string a partir das propriedades do `DateTime`.

### Usando o pacote `intl`

Primeiro, importe o pacote:

```dart
import 'package:intl/intl.dart';
```

Em seguida, formate a data:

```dart
DateTime now = DateTime.now();
String formattedDate = DateFormat('yyyy-MM-dd').format(now);
print(formattedDate); // Saída: 2023-04-12
```

O pacote `intl` permite uma formatação muito mais complexa de forma fácil, incluindo formatos específicos de localidade:

```dart
String formattedDateLocale = DateFormat.yMMMMd('en_US').format(now);
print(formattedDateLocale); // Saída: April 12, 2023
```

Esses exemplos mostram maneiras simples, mas poderosas, de converter e formatar datas em strings em Dart, seja usando a funcionalidade central do Dart ou utilizando o pacote `intl` para opções de formatação mais avançadas.
