---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:41.522532-07:00
description: "\u041A\u0430\u043A: Dart \u043D\u0435 \u0432\u043A\u043B\u044E\u0447\
  \u0430\u0435\u0442 \u0432\u0441\u0442\u0440\u043E\u0435\u043D\u043D\u043E\u0439\
  \ \u043F\u043E\u0434\u0434\u0435\u0440\u0436\u043A\u0438 \u0434\u043B\u044F \u0440\
  \u0430\u0431\u043E\u0442\u044B \u0441 XML \u0432 \u0441\u0432\u043E\u0435\u0439\
  \ \u0441\u0442\u0430\u043D\u0434\u0430\u0440\u0442\u043D\u043E\u0439 \u0431\u0438\
  \u0431\u043B\u0438\u043E\u0442\u0435\u043A\u0435, \u0447\u0442\u043E \u0442\u0440\
  \u0435\u0431\u0443\u0435\u0442 \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\
  \u0430\u043D\u0438\u044F \u0441\u0442\u043E\u0440\u043E\u043D\u043D\u0438\u0445\
  \ \u043F\u0430\u043A\u0435\u0442\u043E\u0432. \u041E\u0434\u0438\u043D \u0438\u0437\
  \u2026"
lastmod: '2024-03-13T22:44:44.556448-06:00'
model: gpt-4-0125-preview
summary: "Dart \u043D\u0435 \u0432\u043A\u043B\u044E\u0447\u0430\u0435\u0442 \u0432\
  \u0441\u0442\u0440\u043E\u0435\u043D\u043D\u043E\u0439 \u043F\u043E\u0434\u0434\u0435\
  \u0440\u0436\u043A\u0438 \u0434\u043B\u044F \u0440\u0430\u0431\u043E\u0442\u044B\
  \ \u0441 XML \u0432 \u0441\u0432\u043E\u0435\u0439 \u0441\u0442\u0430\u043D\u0434\
  \u0430\u0440\u0442\u043D\u043E\u0439 \u0431\u0438\u0431\u043B\u0438\u043E\u0442\u0435\
  \u043A\u0435, \u0447\u0442\u043E \u0442\u0440\u0435\u0431\u0443\u0435\u0442 \u0438\
  \u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u043D\u0438\u044F \u0441\u0442\
  \u043E\u0440\u043E\u043D\u043D\u0438\u0445 \u043F\u0430\u043A\u0435\u0442\u043E\u0432\
  ."
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 XML"
weight: 40
---

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
