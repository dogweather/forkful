---
title:                "Пошук та заміна тексту"
html_title:           "Fish Shell: Пошук та заміна тексту"
simple_title:         "Пошук та заміна тексту"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/fish-shell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Те, що це і для чого?
Заміна тексту - це процес заміни певних частин тексту на інші. Це корисний інструмент для програмістів, які шукають спосіб швидко змінювати текст у своїх програмах або скриптах.

## Як це зробити:
Для початку, вам потрібен Fish Shell, модний та маг Ukrainian style_ для програмістів. Давайте посилатися на це, щоб шукати та замінювати текст в нашому файлі "example.txt":

```Fish Shell
sed 's/hello/привіт/g' example.txt
```
Вивід: цей файл містить "привіт world!"

## Глибший дослідження:
Заміна тексту є важливим елементом в програмуванні з самого початку. Давні програмісті навіть використовували машиноподібні методи, щоб швидко змінювати текст у своїх програмах. Фіш Shell - це один з багатьох способів здійснення заміни тексту. Іншими альтернативами є awk та grep, але Fish Shell є більш простою та зручною для використання опцією.

## Дивись також:
- [Офіційна документація по Fish Shell](https://fishshell.com/docs/current/)
- [Інші корисні команди Fish Shell](https://fishshell.com/docs/current/commands.html)
- [Розділ "Регулярні вирази" у книзі "Володіння регулярними виразами"](https://www.oreilly.com/library/view/9781449312786/ch04.html)