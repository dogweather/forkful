---
title:                "Писання у стандартну помилку"
html_title:           "Javascript: Писання у стандартну помилку"
simple_title:         "Писання у стандартну помилку"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Чому

Напевне ви питаєтеся, чому знадобиться писати в стандартну помилку в Javascript. Однією з основних причин є відладка програм, особливо в складних випадках, коли ви не можете вивести помилку звичайним способом.

## Як це зробити

Існує кілька способів вивести помилку в стандартну помилку в Javascript. Один з них - використовувати функцію `console.error()`. Наприклад:

```Javascript
console.error("Помилка: змінна не оголошена");
```

Це дозволить вам вивести повідомлення про помилку в консолі браузера. Крім того, ви можете використовувати ключове слово `throw` для виведення помилки в стандартну помилку. Наприклад:

```Javascript
throw new Error("Помилка: змінна не оголошена");
```

Цей приклад покаже повідомлення про помилку разом зі стеком викликів у консолі.

## Глибокий розбір

В JavaScript також існує можливість перенаправлення помилки до стандартного потоку помилок (stderr) з використанням потоку `process.stderr`. Наприклад:

```Javascript
process.stderr.write("Помилка: змінна не оголошена");
```

Це може бути корисно для побудови спеціальних логів або для використання разом з іншими інструментами. Докладніше про цей метод можна дізнатися у [документації Node.js](https://nodejs.org/api/process.html#process_a_note_on_process_i_o).

## Дивіться також

- [Документація про виключення в Javascript](https://developer.mozilla.org/uk/docs/web/javascript/reference/errors)
- [Відеоурок про вивід помилок у Javascript](https://www.youtube.com/watch?v=ipr3_FZSmtM)
- [Стаття про відлагодження в Javascript](https://javascript.info/debugging)