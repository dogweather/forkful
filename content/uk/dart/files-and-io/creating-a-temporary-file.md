---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:53.281021-07:00
description: "\u042F\u043A: \u0411\u0456\u0431\u043B\u0456\u043E\u0442\u0435\u043A\
  \u0430 `dart:io` \u0432 Dart \u0441\u043F\u0440\u0438\u044F\u0454 \u0441\u0442\u0432\
  \u043E\u0440\u0435\u043D\u043D\u044E \u0442\u0438\u043C\u0447\u0430\u0441\u043E\u0432\
  \u0438\u0445 \u0444\u0430\u0439\u043B\u0456\u0432 \u0437\u0430 \u0434\u043E\u043F\
  \u043E\u043C\u043E\u0433\u043E\u044E \u043A\u043B\u0430\u0441\u0443 `Directory`.\
  \ \u041E\u0441\u044C \u043F\u0440\u043E\u0441\u0442\u0438\u0439 \u0441\u043F\u043E\
  \u0441\u0456\u0431 \u0441\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F \u0442\
  \u0438\u043C\u0447\u0430\u0441\u043E\u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\
  \u0443 \u0442\u0430\u2026"
lastmod: '2024-03-13T22:44:48.833150-06:00'
model: gpt-4-0125-preview
summary: "\u0411\u0456\u0431\u043B\u0456\u043E\u0442\u0435\u043A\u0430 `dart:io` \u0432\
  \ Dart \u0441\u043F\u0440\u0438\u044F\u0454 \u0441\u0442\u0432\u043E\u0440\u0435\
  \u043D\u043D\u044E \u0442\u0438\u043C\u0447\u0430\u0441\u043E\u0432\u0438\u0445\
  \ \u0444\u0430\u0439\u043B\u0456\u0432 \u0437\u0430 \u0434\u043E\u043F\u043E\u043C\
  \u043E\u0433\u043E\u044E \u043A\u043B\u0430\u0441\u0443 `Directory`."
title: "\u0421\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F \u0442\u0438\u043C\u0447\
  \u0430\u0441\u043E\u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0443"
weight: 21
---

## Як:
Бібліотека `dart:io` в Dart сприяє створенню тимчасових файлів за допомогою класу `Directory`. Ось простий спосіб створення тимчасового файлу та запису деякого вмісту в нього:

```dart
import 'dart:io';

Future<void> main() async {
  // Створити тимчасову директорію (місце розташування специфічно для системи)
  Directory tempDir = await Directory.systemTemp.createTemp('my_temp_dir_');

  // Створити тимчасовий файл у цій директорії
  File tempFile = File('${tempDir.path}/my_temp_file.txt');

  // Записати деякий вміст до тимчасового файлу
  await tempFile.writeAsString('Це якийсь тимчасовий вміст');

  print('Тимчасовий файл створено: ${tempFile.path}');

  // Приклад вивода: Тимчасовий файл створено: /tmp/my_temp_dir_A1B2C3/my_temp_file.txt
}
```

### Використання сторонньої бібліотеки: `path_provider`
Для додатків (особливо мобільних додатків на Flutter) ви можете захотіти створювати тимчасові файли у більш уніфікований і керований спосіб. Пакет `path_provider` може допомогти вам знайти правильну тимчасову директорію на різних платформах (iOS, Android тощо).

Спочатку додайте `path_provider` до вашого `pubspec.yaml` у залежності:

```yaml
dependencies:
  path_provider: ^2.0.9
```

І ось як ви можете використовувати його для створення тимчасового файлу:

```dart
import 'dart:io';
import 'package:path_provider/path_provider.dart';

Future<void> main() async {
  // Отримати тимчасову директорію
  final Directory tempDir = await getTemporaryDirectory();

  // Створити тимчасовий файл у цій директорії
  final File tempFile = File('${tempDir.path}/my_temp_file.txt');

  // Записати деякий вміст до тимчасового файлу
  await tempFile.writeAsString('Це якийсь тимчасовий вміст з path_provider');

  print('Тимчасовий файл створено з path_provider: ${tempFile.path}');

  // Приклад вивода: Тимчасовий файл створено з path_provider: /tmp/my_temp_file.txt (шлях може відрізнятися в залежності від платформи)
}
```

Ці фрагменти ілюструють створення та взаємодію з тимчасовими файлами в Dart, надаючи простий та практичний підхід для управління даними на короткотермінові цілі.
