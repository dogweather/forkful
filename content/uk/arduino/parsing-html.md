---
title:                "Розбір HTML"
html_title:           "Arduino: Розбір HTML"
simple_title:         "Розбір HTML"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/parsing-html.md"
---

{{< edit_this_page >}}

## Що і чому?
Аналіз HTML - це процес розділення HTML-документа на його основні складові, які потім можемо використовувати у програмі. Програмісти це роблять, щоб автоматично екстрагувати або маніпулювати дані, вкладені у HTML.

## Як це зробити:
Ось крутий приклад коду ```Arduino```, який аналізує HTML та виводить результат:

```Arduino
#include <HTMLParser.h>

void setup() {
  Serial.begin(9600);
  String html = "<h1>Привіт, світе!</h1>";
  HTMLParser parser(html);
  String heading = parser.extract("h1");
  
  Serial.println(heading);
}

void loop() {
  // тут пусто
}
```
Якщо все зроблено правильно, ви побачите вивід наступного:

```
Привіт, світе!
```

## Поглиблений занурення:
Історично, аналіз HTML використовувався для створення індексів пошукових систем і сайтів з новинами. Для аналізу HTML є багато альтернатив, включаючи XPath і CSS селектори. Щодо деталей реалізації для Arduino, важливо пам'ятати про обмежені ресурси цих пристроїв, отже, написаний шматок коду треба оптимізувати.

## Дивіться також:
* [ Arduino HTML Parsing Library GitHub Repo](https://github.com/timothyblynjacobs/html-parser)
* [ Посібник по XPath](https://www.devopsschool.com/tutorial/xpath/tutorial/html/)
* [ Стаття про працю із CSS селекторами](https://www.digitalocean.com/community/tutorials/css-selectors-explained)