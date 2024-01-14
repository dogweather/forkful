---
title:                "Elm: Отримання поточної дати"
simple_title:         "Отримання поточної дати"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Чому
Нерідко при програмуванні важливим є вміння працювати з датами та часом. Наприклад, для створення додатків, які дозволяють користувачам переглядати події за датою, або для встановлення термінів дії акційних пропозицій. У цих випадках необхідно вміти отримувати поточну дату. У цій статті ми дізнаємося, як це зробити за допомогою мови програмування Elm.

## Як отримати поточну дату
Один із способів отримати поточну дату в Elm - використати пакет [time](https://package.elm-lang.org/packages/elm/time/latest/), який надає функції для роботи з часом.

```elm
import Time exposing (..)

main =
    let
        now = Time.now
    in
    text (toString now)
```

Цей код поверне поточну дату та час у форматі `Posix`. Варто зазначити, що в результат можуть бути дещо відрізнятися, оскільки він базується на часовій зоні сервера, на якому запущений додаток.

Щоб отримати дату у більш зрозумілому форматі, можна використати функцію `utcTimeToDate`. Наприклад, якщо потрібно отримувати текстове представлення дати у форматі `dd-MM-yyyy`, код може виглядати так:

```elm
import Time exposing (..)

main =
    let
        now = Time.now
        formattedDate = utcTimeToDate "%d-%m-%Y" now
    in
    text formattedDate
```

В результаті на екрані буде виведена поточна дата.

## Глибші дізнання
Якщо вам цікаво детальніше дізнатися про роботу з датами в Elm, ви можете побачити повний список функцій, пов'язаних з часом, у документації пакету [time](https://package.elm-lang.org/packages/elm/time/latest/).

## Дивіться також
- [Офіційна документація Elm](https://guide.elm-lang.org/)
- [Туторіали з Elm на сайті IT Zhytomyr](https://ithub.community/c/elm/20)
- [Курс з Elm на сайті Codecademy](https://www.codecademy.com/learn/learn-elm)