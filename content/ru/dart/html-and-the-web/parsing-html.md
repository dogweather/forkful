---
title:                "Разбор HTML"
date:                  2024-03-08T21:55:58.081239-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Парсинг HTML в программировании включает извлечение данных из HTML-документов. Программисты делают это для взаимодействия с веб-контентом или сбора информации для извлечения данных, тестирования или автоматизации, даже когда официальные API недоступны.

## Как это сделать:
Dart не предоставляет встроенную поддержку для парсинга HTML в своих основных библиотеках. Однако, вы можете использовать сторонний пакет, такой как `html`, для парсинга и манипуляции HTML-документами.

Сначала добавьте пакет `html` в файл `pubspec.yaml`:

```yaml
dependencies:
  html: ^0.15.0
```

Затем импортируйте пакет в ваш Dart файл:

```dart
import 'package:html/parser.dart' show parse;
import 'package:html/dom.dart';
```

Вот базовый пример парсинга строки, содержащей HTML, и извлечения данных:

```dart
void main() {
  var htmlDocument = """
  <html>
    <body>
      <h1>Привет, Dart!</h1>
      <p>Это параграф в примере HTML</p>
    </body>
  </html>
  """;

  // Парсинг HTML строки
  Document document = parse(htmlDocument);

  // Извлечение данных
  String title = document.querySelector('h1')?.text ?? "Заголовок не найден";
  String paragraph = document.querySelector('p')?.text ?? "Параграф не найден";

  print('Заголовок: $title');
  print('Параграф: $paragraph');
}
```

Вывод:

```
Заголовок: Привет, Dart!
Параграф: Это параграф в примере HTML
```

Чтобы взаимодействовать с реальными веб-страницами, вы можете комбинировать парсинг `html` с HTTP-запросами (используя пакет `http` для получения веб-контента). Вот быстрый пример:

Сначала добавьте пакет `http` вместе с `html`:

```yaml
dependencies:
  html: ^0.15.0
  http: ^0.13.3
```

Затем извлеките и проанализируйте HTML-страницу из сети:

```dart
import 'package:http/http.dart' as http;
import 'package:html/parser.dart' show parse;

void main() async {
  var url = 'https://example.com';
  
  // Получение веб-страницы
  var response = await http.get(Uri.parse(url));
  
  if (response.statusCode == 200) {
    var document = parse(response.body);

    // Предположим, что на странице есть интересующие вас теги <h1>
    var headlines = document.querySelectorAll('h1').map((e) => e.text).toList();
    
    print('Заголовки: $headlines');
  } else {
    print('Запрос не удался со статусом: ${response.statusCode}.');
  }
}
```

Примечание: Техника веб-скрапинга, показанная выше, должна использоваться ответственно и в соответствии с условиями использования веб-сайта.
