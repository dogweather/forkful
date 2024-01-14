---
title:                "Haskell: Початок нового проекту"
simple_title:         "Початок нового проекту"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Чому

Найновіший проект - відмінна нагода для розвитку в програмуванні та використання нових технологій.

## Як розпочати

Використовуйте мову програмування Haskell для створення міцного та ефективного проекту. Тут ми наведемо декілька прикладів для початку роботи.

```Haskell
-- Визначення типу даних
data Book = Book {title :: String, author :: String}

-- Змінні
myBook :: Book
myBook = Book "Назва книги" "Автор"

-- Функції
getBookTitle :: Book -> String
getBookTitle (Book title _) = title

-- Виклик функції
getBookTitle myBook

-- Вивід на екран
"Назва книги"
```

## Глибокий погляд

Новий проект може бути викликом, але нехай це не зупиняє вас. Починаючи з добре спроектованої структури даних та функцій, ви можете створити високоякісний програмний продукт.

Серед корисних ресурсів для початку роботи з Haskell є:

- [Офіційний сайт мови Haskell](https://www.haskell.org/)
- [Вступ до мови Haskell на YouTube](https://www.youtube.com/watch?v=nVIPC_2z93c)
- [Основи програмування на мові Haskell](http://learnyouahaskell.com/chapters)
- [Курс з Haskell на Coursera](https://www.coursera.org/courses?query=haskell)
- [Найкращі практики програмування на мові Haskell](https://wiki.haskell.org/Best_practices)


## Дивіться також

- [Основи функціонального програмування в Haskell](http://blog.brainpowered.ro/article/functional-programming-in-Haskell)
- [Створення веб-додатків з використанням Haskell та Yesod](https://www.tweag.io/posts/2013-12-15-yesod-webapp-tutorial.html)
- [Навчальні матеріали для вивчення Haskell](https://github.com/lampepfl/scala/tree/2.13.x/test/files/neg/t1954)
- [Онлайн-спільнота програмістів на мові Haskell](https://stackoverflow.com/questions/tagged/haskell)