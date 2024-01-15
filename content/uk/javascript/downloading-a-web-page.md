---
title:                "Завантаження веб-сторінки."
html_title:           "Javascript: Завантаження веб-сторінки."
simple_title:         "Завантаження веб-сторінки."
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Чому

Завантаження веб-сторінок є важливою частиною програмування в Яваскрипті, оскільки це дозволяє отримати доступ до вмісту в Інтернеті, що відкриває безліч можливостей для створення цікавих і корисних додатків.

## Як

Для завантаження веб-сторінки в Яваскрипті, потрібно виконати наступні кроки:

```javascript
// Створення об'єкта XMLHttpRequest
var xhttp = new XMLHttpRequest();

// Встановлення методу запиту та URL
xhttp.open("GET", "https://www.example.com", true);

// Встановлення колбек функції для обробки відповіді
xhttp.onreadystatechange = function() {
  if (this.readyState == 4 && this.status == 200) {
    // Обробка отриманої відповіді
    var response = this.responseText;
    // Виконання потрібних дій з отриманим вмістом
    console.log(response);
  }
};

// Відправка запиту
xhttp.send();
```
Вище наведений приклад дозволить вам отримати вміст веб-сторінки та використати його для створення потрібної функціональності.

## Глибинне занурення

Завантаження веб-сторінок може бути використано для взаємодії з іншими ресурсами в Інтернеті, такими як API, бази даних та інші додатки. Яваскрипт пропонує різні методи та бібліотеки для спрощення процесу завантаження та обробки вмісту веб-сторінок.

## Дивись також

- [MDN - XMLHttpRequest](https://developer.mozilla.org/uk/docs/Web/API/XMLHttpRequest)
- [Асинхронні запити в Яваскрипт](https://learn.javascript.ru/xmlhttprequest-xmlhttprequest)