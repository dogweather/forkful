---
title:                "Javascript: Завантаження веб-сторінки"
simple_title:         "Завантаження веб-сторінки"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Чому

Одним з основних елементів веб-розробки є звантаження веб-сторінки. Цей процес дозволяє отримувати дані з Інтернету і використовувати їх для подальших дій, таких як аналіз та відображення інформації. Для українських розробників важливо знати, як це зробити за допомогою Javascript, щоб розширити можливості своїх програм та веб-сайтів.

## Як це зробити

Для початку створимо змінну `url`, в яку будемо передавати адресу веб-сторінки, яку хочемо завантажити. Потім створимо об'єкт `XMLHttpRequest` та викличемо метод `open()`, вказавши тип запиту та адресу веб-сторінки. Для отримання результату викличемо метод `send()` та передамо його значення в змінну `response`. Нижче наведений приклад коду:

```Javascript
let url = "https://example.com"; // задаемо посилання на сторінку, яку хочемо завантажити
let xhr = new XMLHttpRequest(); // створюємо об'єкт XMLHttpRequest
xhr.open('GET', url); // вказуємо тип запиту та адресу сторінки
xhr.send(); // виконуємо запит та отримуємо результат

// використовуємо отриманий результат
console.log(xhr.response); // виводимо результат у консолі
document.getElementById("output").innerHTML = xhr.response; // виводимо результат у веб-сторінці
```

Цей приклад допоможе вам зрозуміти, як працює звантаження веб-сторінки за допомогою Javascript.

## Глибше

У процесі звантаження веб-сторінки, Javascript створює запит до сервера та отримує з нього відповідь у вигляді тексту, коду чи іншої інформації. Ця відповідь може бути використана для подальшої обробки або відображення. Також, важливо враховувати можливість помилок під час звантаження, тому доцільно додати обробник подій `onreadystatechange`, який буде перевіряти стан запиту та обробляти його результат. 

## Дивись також

1. [MDN - XMLHttpRequest](https://developer.mozilla.org/uk/docs/Web/API/XMLHttpRequest)
2. [W3Schools - XMLHttpRequest](https://www.w3schools.com/xml/xml_http.asp)
3. [Книга "Javascript для дітей" - розділ 6](https://books.google.com.ua/books?id=ARVxDwAAQBAJ&printsec=frontcover&dq=%D0%B4%D1%96%D1%82%D0%B5%D0%B9+javascript&hl=uk&sa=X&ved=0ahUKEwjsvafCgeLqAhXWr4sKHcY3DKoQ6AEIKDAA#v=onepage&q=%D0%B7%D0%B2