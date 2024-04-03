---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:11.156231-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0411\u0438\u0431\u043B\u0438\u043E\u0442\u0435\u043A\u0430 `dart:io`\
  \ \u0432 Dart \u043E\u0431\u043B\u0435\u0433\u0447\u0430\u0435\u0442 \u0441\u043E\
  \u0437\u0434\u0430\u043D\u0438\u0435 \u0432\u0440\u0435\u043C\u0435\u043D\u043D\u044B\
  \u0445 \u0444\u0430\u0439\u043B\u043E\u0432 \u0447\u0435\u0440\u0435\u0437 \u043A\
  \u043B\u0430\u0441\u0441 `Directory`. \u0412\u043E\u0442 \u043F\u0440\u043E\u0441\
  \u0442\u043E\u0439 \u0441\u043F\u043E\u0441\u043E\u0431 \u0441\u043E\u0437\u0434\
  \u0430\u0442\u044C \u0432\u0440\u0435\u043C\u0435\u043D\u043D\u044B\u0439 \u0444\
  \u0430\u0439\u043B \u0438\u2026"
lastmod: '2024-03-13T22:44:44.547874-06:00'
model: gpt-4-0125-preview
summary: "\u0411\u0438\u0431\u043B\u0438\u043E\u0442\u0435\u043A\u0430 `dart:io` \u0432\
  \ Dart \u043E\u0431\u043B\u0435\u0433\u0447\u0430\u0435\u0442 \u0441\u043E\u0437\
  \u0434\u0430\u043D\u0438\u0435 \u0432\u0440\u0435\u043C\u0435\u043D\u043D\u044B\u0445\
  \ \u0444\u0430\u0439\u043B\u043E\u0432 \u0447\u0435\u0440\u0435\u0437 \u043A\u043B\
  \u0430\u0441\u0441 `Directory`."
title: "\u0421\u043E\u0437\u0434\u0430\u043D\u0438\u0435 \u0432\u0440\u0435\u043C\u0435\
  \u043D\u043D\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0430"
weight: 21
---

## Как это сделать:
Библиотека `dart:io` в Dart облегчает создание временных файлов через класс `Directory`. Вот простой способ создать временный файл и записать в него некоторые данные:

```dart
import 'dart:io';

Future<void> main() async {
  // Создать временный каталог (местоположение, специфичное для системы)
  Directory tempDir = await Directory.systemTemp.createTemp('my_temp_dir_');

  // Создать временный файл в этом каталоге
  File tempFile = File('${tempDir.path}/my_temp_file.txt');

  // Записать некоторые данные в временный файл
  await tempFile.writeAsString('Это некоторое временное содержимое');

  print('Временный файл создан: ${tempFile.path}');

  // Пример вывода: Временный файл создан: /tmp/my_temp_dir_A1B2C3/my_temp_file.txt
}
```

### Использование сторонней библиотеки: `path_provider`
Для приложений (особенно мобильных приложений с Flutter) вы можете захотеть создавать временные файлы более унифицированным и управляемым способом. Пакет `path_provider` может помочь вам найти правильный временный каталог на разных платформах (iOS, Android и т.д.).

Сначала добавьте `path_provider` в ваш `pubspec.yaml` в раздел зависимостей:

```yaml
dependencies:
  path_provider: ^2.0.9
```

И вот как вы можете использовать его для создания временного файла:

```dart
import 'dart:io';
import 'package:path_provider/path_provider.dart';

Future<void> main() async {
  // Получить временный каталог
  final Directory tempDir = await getTemporaryDirectory();

  // Создать временный файл в этом каталоге
  final File tempFile = File('${tempDir.path}/my_temp_file.txt');

  // Записать некоторые данные в временный файл
  await tempFile.writeAsString('Это некоторое временное содержимое с path_provider');

  print('Временный файл создан с path_provider: ${tempFile.path}');

  // Пример вывода: Временный файл создан с path_provider: /tmp/my_temp_file.txt (путь может отличаться в зависимости от платформы)
}
```

Эти фрагменты иллюстрируют создание и взаимодействие с временными файлами в Dart, предоставляя простой и практичный подход для управления данными в краткосрочных целях.
