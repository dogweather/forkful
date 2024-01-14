---
title:    "Gleam: Капіталізація рядка"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Для чого: 

У програмуванні часто виникає потреба змінити регістр окремих символів у рядку. Наприклад, для виведення імен користувачів у капіталізованій формі. В цьому випадку функція capitalise допоможе зробити це ефективно та швидко.

## Як використовувати:

Для початку потрібно імпортувати модуль `strings` та виконати наступну команду:

```Gleam
capitalised_string = strings.capitalise("hello world")
```

Результатом цього буде рядок `Hello world`, з першою буквою у верхньому регістрі.

## Глибше копті специфікації:

Функція `capitalise` приймає рядок як аргумент та повертає його копію з першою буквою кожного слова у верхньому регістрі. Також, вона ігнорує решту символів у рядку, залишаючи їх у незмінному стані.

## Дивись також:

* Офіційна документація для функції capitalise: https://gleam.run/modules/strings.html#capitalise
* Приклади використання рядкових функцій у Gleam: https://medium.com/@gleam_language/working-with-strings-in-gleam-8a0305742baf
* Онлайн гайд з Gleam для початківців: https://github.com/gleam-lang/gleam/blob/master/docs/getting_started.md

## Спостерігайте за оновленнями:

Ви можете слідкувати за оновленнями щодо мови програмування Gleam на офіційному сайті: https://gleam.run/ або у соціальних мережах Gleam Twitter: https://twitter.com/gleamlang та Gleam subreddit: https://www.reddit.com/r/gleamlang/.