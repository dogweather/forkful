---
date: 2024-01-26 04:28:04.843017-07:00
description: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 XML \u043D\u0430 Arduino\
  \ \u0432\u043A\u043B\u044E\u0447\u0430\u0454 \u0430\u043D\u0430\u043B\u0456\u0437\
  \ \u0456 \u043C\u0430\u043D\u0456\u043F\u0443\u043B\u044F\u0446\u0456\u044E \u0434\
  \u0430\u043D\u0438\u043C\u0438 XML, \u044F\u043A\u0456 \u0437\u0430\u0437\u0432\u0438\
  \u0447\u0430\u0439 \u043D\u0430\u0434\u0445\u043E\u0434\u044F\u0442\u044C \u0437\
  \ \u0432\u0435\u0431-API \u0430\u0431\u043E \u0444\u0430\u0439\u043B\u0456\u0432\
  \ \u043A\u043E\u043D\u0444\u0456\u0433\u0443\u0440\u0430\u0446\u0456\u0457. \u041F\
  \u0440\u043E\u0433\u0440\u0430\u043C\u0456\u0441\u0442\u0438 \u0440\u043E\u0431\u043B\
  \u044F\u0442\u044C \u0446\u0435, \u0449\u043E\u0431\u2026"
lastmod: '2024-03-13T22:44:49.809880-06:00'
model: gpt-4-0125-preview
summary: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 XML \u043D\u0430 Arduino \u0432\
  \u043A\u043B\u044E\u0447\u0430\u0454 \u0430\u043D\u0430\u043B\u0456\u0437 \u0456\
  \ \u043C\u0430\u043D\u0456\u043F\u0443\u043B\u044F\u0446\u0456\u044E \u0434\u0430\
  \u043D\u0438\u043C\u0438 XML, \u044F\u043A\u0456 \u0437\u0430\u0437\u0432\u0438\u0447\
  \u0430\u0439 \u043D\u0430\u0434\u0445\u043E\u0434\u044F\u0442\u044C \u0437 \u0432\
  \u0435\u0431-API \u0430\u0431\u043E \u0444\u0430\u0439\u043B\u0456\u0432 \u043A\u043E\
  \u043D\u0444\u0456\u0433\u0443\u0440\u0430\u0446\u0456\u0457. \u041F\u0440\u043E\
  \u0433\u0440\u0430\u043C\u0456\u0441\u0442\u0438 \u0440\u043E\u0431\u043B\u044F\u0442\
  \u044C \u0446\u0435, \u0449\u043E\u0431\u2026"
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 XML"
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
