---
title:                "Друк відлагоджувального виводу"
html_title:           "Arduino: Друк відлагоджувального виводу"
simple_title:         "Друк відлагоджувального виводу"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/printing-debug-output.md"
---

{{< edit_this_page >}}

## Що і чому?

Вивід налагодження - це механізм, яким програмісти визначають проблеми у своєму коді, виводячи інформацію в консолі чи в інших місцях. Це важливо, оскільки це допомагає відслідковувати та виправляти помилки, що звичайно можуть бути складно розгледіти.

## Як це зробити?

Ось приклад виведення налагодження в Javascript:

```Javascript
console.log('Це повідомлення для налагодження');

let variable = 5;
console.log('Значення моєї змінної:', variable);
```

У вікні консолі ви побачите:

```
Це повідомлення для налагодження
Значення моєї змінної: 5
```
## Поглиблено

Історично, console.log був одним з перших інструментів для виводу налагодження у Javascript. Він досі широко використовується, але в сучасному програмуванні також є багато альтернатив, таких як засоби налагодження браузерів та інтегровані середовища розробки (IDE).

Основна реалізація console.log проста: вона виводить деталі до консолі, що можуть бути переглянуті програмістом.

## Див. ще

[MDN - debug output](https://developer.mozilla.org/uk/docs/Learn/JavaScript/First_steps/What_went_wrong)
[Chrome DevTools - JavaScript Debugging Reference](https://developers.google.com/web/tools/chrome-devtools/javascript/reference)