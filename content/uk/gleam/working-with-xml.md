---
title:                "Робота з XML"
date:                  2024-01-26T04:31:35.230121-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з XML"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/gleam/working-with-xml.md"
---

{{< edit_this_page >}}

## Що і чому?
Робота з XML передбачає аналіз, маніпуляції та створення документів XML, які використовуються для обміну даними через їх структурований та широко розповсюджений формат. Програмісти обробляють XML для інтерфейсу з безліччю систем, де XML є лінгва франка даних.

## Як це зробити:
Gleam не підтримує XML нативно, тому ми використаємо зовнішню бібліотеку, як от `gleam_xml`. Спочатку додайте її до файлу `gleam.toml`:

```toml
[dependencies]
gleam_xml = "~> 1.0"
```

Тепер аналізуйте та створюйте XML:

```rust
import gleam/xml

// Аналіз XML
let doc = xml.parse("<note><to>Tove</to><from>Jani</from></note>")?

// Створення XML
let node = xml.Element(
  "note",
  [],
  [
    xml.Element("to", [], [xml.Text("Tove")]),
    xml.Element("from", [], [xml.Text("Jani")])
  ]
)
let xml_string = xml.render(node)
```

Приклад виводу для `xml.render(node)`:

```xml
<note><to>Tove</to><from>Jani</from></note>
```

## Поглиблений огляд
XML означає eXtensible Markup Language, специфікація від W3C як сестра HTML. Вона існує з кінця 90-х років. Для Gleam обробка XML здається кроком назад у часі. JSON та Protocol Buffers є моднішими, але широке використання XML у спадкових системах та певних галузях означає, що він все ще актуальний.

Існують альтернативи, такі як `xmerl` у екосистемі Erlang; однак, бібліотека `gleam_xml` надає більш ідіоматичний підхід для користувачів Gleam. Вона побудована на основі існуючих бібліотек Erlang, але надає інтерфейс, дружній до Gleam. Підхід Gleam до XML прагне до простоти та безпеки, зменшуючи стандартний код і акцентуючи на безпеці типів.

З точки зору реалізації, бібліотеки XML, включаючи `gleam_xml`, зазвичай надають структури, подібні до DOM. Це передбачає вузли, атрибути та вкладені елементи, використовуючи відповідність шаблонів і моделі конкурентності Erlang для обробки потенційно великих та складних документів.

## Дивіться також
- Бібліотека `gleam_xml` на Hex: [https://hex.pm/packages/gleam_xml](https://hex.pm/packages/gleam_xml)
- Офіційний стандарт XML від W3C: [https://www.w3.org/XML/](https://www.w3.org/XML/)
- Всебічний підручник по XML: [https://www.w3schools.com/xml/](https://www.w3schools.com/xml/)
- Документація `xmerl` Erlang для обробки XML: [http://erlang.org/doc/apps/xmerl/](http://erlang.org/doc/apps/xmerl/)
