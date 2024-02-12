---
title:                "Робота з XML"
aliases: - /uk/arduino/working-with-xml.md
date:                  2024-01-26T04:28:04.843017-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/working-with-xml.md"
---

{{< edit_this_page >}}

## Що і чому?
Робота з XML на Arduino включає аналіз і маніпуляцію даними XML, які зазвичай надходять з веб-API або файлів конфігурації. Програмісти роблять це, щоб інтегруватися з сервісами, що використовують XML для обміну даними, або для зберігання даних у структурованому, зрозумілому для людини форматі.

## Як це зробити:
Ми будемо використовувати бібліотеку `XMLWriter` для створення XML та бібліотеку `tinyxml2` для її аналізу. Спочатку встановіть бібліотеки через Менеджер бібліотек у вашому Arduino IDE.

Створення XML-документа:

```Arduino
#include <XMLWriter.h>

void setup() {
  Serial.begin(9600);
  
  XMLWriter xml(&Serial); // Використання Serial для виводу
  
  xml.header();
  xml.tag("greeting").tag("text").text("Привіт, світ!").close().close();
  xml.flush();
}

void loop() {
}
```

Декодування XML-рядка:

```Arduino
#include <tinyxml2.h>

tinyxml2::XMLDocument doc;
doc.Parse("<greeting><text>Привіт, світ!</text></greeting>");

tinyxml2::XMLElement* text = doc.FirstChildElement("greeting")->FirstChildElement("text");
if (text != nullptr) {
  Serial.println(text->GetText());
}
```

Приклад виводу:

```
<greeting>
  <text>Привіт, світ!</text>
</greeting>
```

## Поглиблено
XML, або Розширювана Мова Розмітки, — це мова розмітки, що визначає набір правил для кодування документів у форматі, який є одночасно читабельним для людини й машини. Вона існує з кінця 90-х років і широко використовується в різних сферах, особливо там, де потрібен обмін даними незалежно від платформи. Обмежені ресурси пам'яті Arduino роблять роботу з XML складнішою, ніж на ПК. Тому життєво важливі легковажні бібліотеки. Хоча JSON набув популярності для обміну даними завдяки своєму простішому синтаксису та меншому розміру, XML все ще широко використовується, особливо при роботі зі старими системами або додатками, які вимагають валідації документів через схеми. Ключ до реалізації XML на Arduino — це аналіз потоку, який читає документ по частинах, щоб зберегти низьке використання пам'яті.

## Дивіться також
- [Документація бібліотеки TinyXML-2](https://leethomason.github.io/tinyxml2/)
- [Бібліотека Arduino JSON](https://arduinojson.org/) як альтернатива при роботі з даними JSON.
- [XML Туторіал W3Schools](https://www.w3schools.com/xml/) для загального вивчення XML.
- [W3C Специфікація XML](https://www.w3.org/XML/) для офіційних стандартів і рекомендацій XML.
