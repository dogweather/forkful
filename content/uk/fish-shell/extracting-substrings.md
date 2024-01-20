---
title:                "Видобування підрядків"
html_title:           "C++: Видобування підрядків"
simple_title:         "Видобування підрядків"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Що та чому?
Робота з підрядками -- це процес видобування певної частини рядка. Програмісти роблять це, щоб ефективно працювати з рядками даних, наприклад при роботі з користувацьким вводом, файлами тощо.

## Як це зробити:
Fish Shell пропонує декілька різних способів вилучення підрядків. Ось кілька прикладів:

```Fish Shell
set string "Привіт, світ!"
echo $string[1 6] // виведе "Привіт"
```

```Fish Shell
set string "Привіт, світ!"
set substring (echo $string | awk '{print substr($0,1,6)}')
echo $substring // виведе "Привіт"
```

## Занурення в деталі:
Вилучення підрядків було впроваджено в багатьох мовах програмування ще з пізніх 70-х. Використовуючи Fish Shell, ви можете вилучити підрядки, використовуючи індекс, або використовувати інструменти, такі як awk.
Незважаючи на це, якщо отримати підрядок має велику важливість для вашого проекту, можливо, для вас буде кращим вибрати мову програмування з більш широкіми можливостями обробки рядків.

## Див. також:
1. [Офіційна документація Fish Shell](https://fishshell.com/docs/current/index.html)
2. [Робота з строками в Fish](https://fishshell.com/docs/current/commands.html#string)
3. [Туторіали Fish Shell](https://fishshell.com/docs/current/tutorial.html)