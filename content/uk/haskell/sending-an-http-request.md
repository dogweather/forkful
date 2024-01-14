---
title:                "Haskell: Відправка HTTP-запиту"
simple_title:         "Відправка HTTP-запиту"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Чому ##

Відправлення HTTP запитів є важливою частиною програмування веб-додатків та API. Вони дозволяють отримувати та обмінюватися даними з сервером, що є необхідним для багатьох сучасних додатків та сервісів. 

## Як це зробити ##

Для відправлення HTTP запитів використовується бібліотека "http-conduit" у Haskell. Спочатку необхідно імпортувати цю бібліотеку у ваш проект:

```Haskell
import Network.HTTP.Conduit
```

Тепер можна створити функцію, яка буде відправляти запити та отримувати відповіді з сервера:

```Haskell
getRequest :: String -> IO String
getRequest url = do
    request' <- parseUrlThrow url
    response <- httpLbs request'
    return $ responseBody response
```

Ця функція приймає URL-адресу (за допомогою типу даних String) та повертає IO String, що означає, що вона відправляє запит та повертає результат у форматі рядка. Спочатку вона використовує функцію "parseUrlThrow" для створення запиту за заданою адресою, а потім відправляє його за допомогою функції "httpLbs". Наприкінці функція повертає тіло відповіді у форматі рядка.

Тепер можна викликати цю функцію та передати їй URL-адресу для отримання відповіді:

```Haskell
main = do
    response <- getRequest "https://example.com"
    print response
```

В цьому прикладі ми друкуємо отриману відповідь у консоль.

## Глибше {{

HTTP запити дозволяють також використовувати різні методи, такі як GET, POST, PUT та DELETE. При отриманні відповіді, ці методи також приймають тіло запиту. Наприклад, якщо ви хочете відправити POST-запит з деякими даними, ви можете зробити наступне:

```Haskell
postRequest :: String -> String -> IO String
postRequest url body = do
    request' <- parseUrlThrow url
    let request = urlEncodedBody [("param", body)] request'
    response <- httpLbs request
    return $ responseBody response
```

Ця функція приймає URL-адресу та тіло запиту (яке може бути представлене у форматі рядка) та повертає відповідь у форматі рядка.

## Дивитися також

- [Haskell: відновлення з Python](https://habr.com/ru/post/505658/)
- [Неофіційний посібник з Haskell для програмістів Python](https://www.parsonsmatt.org/2017/01/06/haskell_vs_python.html)
- [Базовий підручник з Haskell](https://www.seas.upenn.edu/~cis194/fall14/)
- [Haskell на роз-кішці](