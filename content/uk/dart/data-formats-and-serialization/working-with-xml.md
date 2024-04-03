---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:58:04.368226-07:00
description: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 XML \u0432 Dart \u0432\u043A\
  \u043B\u044E\u0447\u0430\u0454 \u0430\u043D\u0430\u043B\u0456\u0437, \u0437\u0430\
  \u043F\u0438\u0442\u0438 \u0442\u0430 \u043C\u043E\u0434\u0438\u0444\u0456\u043A\
  \u0430\u0446\u0456\u044E XML-\u0434\u043E\u043A\u0443\u043C\u0435\u043D\u0442\u0456\
  \u0432, \u043F\u0440\u043E\u0446\u0435\u0441, \u0449\u043E \u0454 \u043A\u0440\u0438\
  \u0442\u0438\u0447\u043D\u043E \u0432\u0430\u0436\u043B\u0438\u0432\u0438\u043C\
  \ \u0434\u043B\u044F \u0437\u0430\u0441\u0442\u043E\u0441\u0443\u043D\u043A\u0456\
  \u0432, \u044F\u043A\u0456 \u0432\u0437\u0430\u0454\u043C\u043E\u0434\u0456\u044E\
  \u0442\u044C \u0437 \u0432\u0435\u0431-\u0441\u0435\u0440\u0432\u0456\u0441\u0430\
  \u043C\u0438,\u2026"
lastmod: '2024-03-13T22:44:48.841324-06:00'
model: gpt-4-0125-preview
summary: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 XML \u0432 Dart \u0432\u043A\
  \u043B\u044E\u0447\u0430\u0454 \u0430\u043D\u0430\u043B\u0456\u0437, \u0437\u0430\
  \u043F\u0438\u0442\u0438 \u0442\u0430 \u043C\u043E\u0434\u0438\u0444\u0456\u043A\
  \u0430\u0446\u0456\u044E XML-\u0434\u043E\u043A\u0443\u043C\u0435\u043D\u0442\u0456\
  \u0432, \u043F\u0440\u043E\u0446\u0435\u0441, \u0449\u043E \u0454 \u043A\u0440\u0438\
  \u0442\u0438\u0447\u043D\u043E \u0432\u0430\u0436\u043B\u0438\u0432\u0438\u043C\
  \ \u0434\u043B\u044F \u0437\u0430\u0441\u0442\u043E\u0441\u0443\u043D\u043A\u0456\
  \u0432, \u044F\u043A\u0456 \u0432\u0437\u0430\u0454\u043C\u043E\u0434\u0456\u044E\
  \u0442\u044C \u0437 \u0432\u0435\u0431-\u0441\u0435\u0440\u0432\u0456\u0441\u0430\
  \u043C\u0438, \u0444\u0430\u0439\u043B\u0430\u043C\u0438 \u043A\u043E\u043D\u0444\
  \u0456\u0433\u0443\u0440\u0430\u0446\u0456\u0457 \u0430\u0431\u043E \u0437\u0430\
  \u0441\u0442\u0430\u0440\u0456\u043B\u0438\u043C\u0438 \u0441\u0438\u0441\u0442\u0435\
  \u043C\u0430\u043C\u0438."
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 XML"
weight: 40
---

## Що та Чому?

Робота з XML в Dart включає аналіз, запити та модифікацію XML-документів, процес, що є критично важливим для застосунків, які взаємодіють з веб-сервісами, файлами конфігурації або застарілими системами. Програмісти роблять це, щоб уможливити обмін даними, конфігурації або навіть віддалені виклики процедур у структурованому, ієрархічному форматі, який є одночасно зрозумілим для людини та придатним для машинної обробки.

## Як:

Dart не включає вбудовану підтримку для роботи з XML у своїй стандартній бібліотеці, що потребує використання сторонніх пакетів. Один із популярних пакетів - `xml`. Для його використання спочатку вам потрібно додати його до вашого `pubspec.yaml`:

```yaml
dependencies:
  xml: ^5.0.0 // Використовуйте останньо доступну версію
```

Потім імпортуйте пакет у ваш Dart файл:

```dart
import 'package:xml/xml.dart' as xml;
```

**Аналіз XML:**

Припустимо, у вас є рядок XML, як це:

```xml
<String name="greeting">Привіт, світ!</String>
```

Ви можете аналізувати та читати XML наступним чином:

```dart
void parseXml(String xmlString) {
    final document = xml.XmlDocument.parse(xmlString);
    final String content = document.findElements('String').single.getAttribute('name');
    print(content); // Вивід: greeting
}

void main() {
  final xmlString = '<String name="greeting">Привіт, світ!</String>';
  parseXml(xmlString);
}
```

**Створення XML-документів:**

Створення нового XML-документа є простим з пакетом `xml`:

```dart
void createXml() {
  final builder = xml.XmlBuilder();
  builder.processing('xml', 'version="1.0"');
  builder.element('greeting', nest: () {
    builder.attribute('name', 'hello');
    builder.text('Привіт, світ!');
  });
  final xmlDocument = builder.buildDocument();
  print(xmlDocument.toXmlString(pretty: true));
}

void main() {
  createXml();
}
```

**Вивід**:

```xml
<?xml version="1.0"?>
<greeting name="hello">Привіт, світ!</greeting>
```

**Запити та Модифікація XML:**

Для пошуку або зміни елементів ви можете використовувати методи, подібні до XPath:

```dart
void modifyXml(String xmlString) {
    var document = xml.XmlDocument.parse(xmlString);
    var greeting = document.findAllElements('greeting').first;
    
    // Модифікація атрибуту 'name'
    greeting.setAttribute('name', 'greeting_modified');
    
    // Додавання нового дочірнього елемента
    greeting.children.add(xml.XmlElement(xml.XmlName('message'), [], [xml.XmlText('До побачення!')]));
    
    print(document.toXmlString(pretty: true));
}

void main() {
  final xmlString = '<greeting name="hello">Привіт, світ!</greeting>';
  modifyXml(xmlString);
}
```

**Вивід**:

```xml
<greeting name="greeting_modified">
  Привіт, світ!
  <message>До побачення!</message>
</greeting>
```

Ці приклади демонструють основні операції для роботи з XML в Dart. З пакетом `xml` ви можете аналізувати, створювати та маніпулювати XML-документами, щоб задовольнити вимоги вашого застосунку.
