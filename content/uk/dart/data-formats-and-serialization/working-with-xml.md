---
title:                "Робота з XML"
date:                  2024-03-08T21:58:04.368226-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

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
