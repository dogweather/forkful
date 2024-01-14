---
title:                "Haskell: Використання регулярних виразів"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Чому

В програмуванні часто зустрічаються ситуації, де потрібно шукати і обробляти певні шаблони в тексті. У таких випадках використання регулярних виразів може значно полегшити завдання. Завдяки регулярним виразам можна здійснювати пошук та заміну певних шаблонів із вказівкою їхнього формату.

## Як

Для використання регулярних виразів у Haskell потрібно імпортувати модуль `Text.Regex.Posix`. Далі можна використовувати функції з цього модуля, такі як `matchRegex` для знаходження відповідності шаблону та `subRegex` для заміни підрядку на інший.

```Haskell
import Text.Regex.Posix

-- Перевірка чи міститься слово у тексті
matchRegex "hello" "Hello, world!" -- Just "hello"

-- Заміна усіх чисел на текст "number"
subRegex (mkRegex "[0-9]+") "number" "There are 123 numbers." -- "There are number numbers."
```

## Глибока підготовка

Регулярні вирази у Haskell є потужним інструментом для пошуку та обробки тексту. Для складніших задач потрібно звертатися до детальнішої документації, де можна знайти багато корисних функцій і прикладів використання.

## Дивіться також

- [Regex tutorial](https://www.regular-expressions.info/tutorial.html) (англійською)
- [Навчальні матеріали з Haskell](http://www.inf.elte.hu/~spataki/languages/languages.html) (англійською)