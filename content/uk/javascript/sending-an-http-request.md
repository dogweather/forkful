---
title:                "Надіслання HTTP запиту"
html_title:           "Javascript: Надіслання HTTP запиту"
simple_title:         "Надіслання HTTP запиту"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Чому
 Надсилання HTTP запиту є необхідною частиною створення веб-сайтів та додатків. Відправляючи запити до сервера, можна отримати необхідні дані для побудови інтерактивного досвіду для користувачів.

## Як це зробити
Отримання даних від сервера здійснюється за допомогою об'єкту XMLHttpRequest. Нижче представлені приклади коду та виводу, щоб показати як відправляти HTTP запити за допомогою Javascript.

 ```Javascript
// створюємо новий об'єкт XMLHttpRequest
var xhr = new XMLHttpRequest();

// встановлюємо тип запиту та URL
xhr.open('GET', 'http://www.example.com/api/data', true);

// встановлюємо обробник подій для обробки результатів запиту
xhr.onload = function() {
  // перевіряємо стан запиту (200 - успішний запит, 404 - сторінка не знайдена)
  if (xhr.status === 200) {
    // парсимо отримані дані з сервера
    var data = JSON.parse(xhr.responseText);
    // використовуємо дані для побудови сторінки
    renderPage(data);
  } else {
    console.log('Помилка запиту! Код статусу: ' + xhr.status);
  }
};

// в передачу запиту на сервер
xhr.send();
```
Вивод:
```
{
  "id": 123,
  "name": "John",
  "age": 25
}
``` 

## Глибоке погруження
Але що стоїть за відправленням HTTP запита? Коли ми викликаємо метод `send()` на об'єкті XMLHttpRequest, він створює новий потік для передачі даних до сервера за допомогою HTTP протоколу. Коли сервер отримує запит, він обробляє його та повертає відповідь з даними, які ми можемо отримати за допомогою властивості `responseText`.

## Дивіться також
- [MDN: XMLHttpRequest](https://developer.mozilla.org/uk/docs/Web/API/XMLHttpRequest)
- [ТРІАДА: Відправка HTTP-запитів з Javascript](https://try.t3devs.com/2817253?lang=uk)