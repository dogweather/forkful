---
title:                "Работа с XML"
date:                  2024-03-08T21:57:41.522532-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

Работа с XML в Dart включает в себя анализ, запрос и изменение XML-документов, что является критически важным для приложений, взаимодействующих с веб-сервисами, файлами конфигурации или наследуемыми системами. Программисты делают это для обеспечения обмена данными, конфигураций или даже удаленных вызовов процедур в структурированном иерархическом формате, который одновременно удобочитаем и доступен для машинного анализа.

## Как:

Dart не включает встроенной поддержки для работы с XML в своей стандартной библиотеке, что требует использования сторонних пакетов. Один из популярных пакетов - `xml`. Чтобы использовать его, сначала нужно добавить его в ваш `pubspec.yaml`:

```yaml
dependencies:
  xml: ^5.0.0 // Используйте последнюю доступную версию
```

Затем импортируйте пакет в ваш Dart файл:

```dart
import 'package:xml/xml.dart' as xml;
```

**Анализ XML:**

Представим, у вас есть строка XML вида:

```xml
<String name="greeting">Привет, мир!</String>
```

Вы можете анализировать и читать XML следующим образом:

```dart
void parseXml(String xmlString) {
    final document = xml.XmlDocument.parse(xmlString);
    final String content = document.findElements('String').single.getAttribute('name');
    print(content); // Выводит: greeting
}

void main() {
  final xmlString = '<String name="greeting">Привет, мир!</String>';
  parseXml(xmlString);
}
```

**Создание XML-документов:**

Создание нового XML-документа просто с пакетом `xml`:

```dart
void createXml() {
  final builder = xml.XmlBuilder();
  builder.processing('xml', 'version="1.0"');
  builder.element('greeting', nest: () {
    builder.attribute('name', 'hello');
    builder.text('Привет, мир!');
  });
  final xmlDocument = builder.buildDocument();
  print(xmlDocument.toXmlString(pretty: true));
}

void main() {
  createXml();
}
```

**Вывод**:

```xml
<?xml version="1.0"?>
<greeting name="hello">Привет, мир!</greeting>
```

**Запрос и изменение XML:**

Для поиска или изменения элементов вы можете использовать методы, похожие на XPath:

```dart
void modifyXml(String xmlString) {
    var document = xml.XmlDocument.parse(xmlString);
    var greeting = document.findAllElements('greeting').first;
    
    // Изменяем атрибут 'name'
    greeting.setAttribute('name', 'greeting_modified');
    
    // Добавляем новый дочерний элемент
    greeting.children.add(xml.XmlElement(xml.XmlName('message'), [], [xml.XmlText('До свидания!')]));
    
    print(document.toXmlString(pretty: true));
}

void main() {
  final xmlString = '<greeting name="hello">Привет, мир!</greeting>';
  modifyXml(xmlString);
}
```

**Вывод**:

```xml
<greeting name="greeting_modified">
  Привет, мир!
  <message>До свидания!</message>
</greeting>
```

Эти примеры демонстрируют основные операции работы с XML в Dart. С пакетом `xml` вы можете анализировать, создавать и манипулировать XML-документами, чтобы удовлетворить требования вашего приложения.
