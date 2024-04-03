---
date: 2024-01-20 17:40:48.071109-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u0423 Haskell, \u0441\u0442\u0432\u043E\u0440\u0438\u0442\u0438 \u0442\u0438\u043C\
  \u0447\u0430\u0441\u043E\u0432\u0438\u0439 \u0444\u0430\u0439\u043B \u043C\u043E\
  \u0436\u043D\u0430 \u0437\u0430 \u0434\u043E\u043F\u043E\u043C\u043E\u0433\u043E\
  \u044E \u043F\u0430\u043A\u0435\u0442\u0443 `temporary`. \u0421\u043F\u0435\u0440\
  \u0448\u0443 \u043F\u043E\u0442\u0440\u0456\u0431\u043D\u043E \u0432\u0441\u0442\
  \u0430\u043D\u043E\u0432\u0438\u0442\u0438 \u043F\u0430\u043A\u0435\u0442 \u0447\
  \u0435\u0440\u0435\u0437 `cabal` \u0430\u0431\u043E `stack`."
lastmod: '2024-03-13T22:44:49.393209-06:00'
model: gpt-4-1106-preview
summary: "\u0423 Haskell, \u0441\u0442\u0432\u043E\u0440\u0438\u0442\u0438 \u0442\u0438\
  \u043C\u0447\u0430\u0441\u043E\u0432\u0438\u0439 \u0444\u0430\u0439\u043B \u043C\
  \u043E\u0436\u043D\u0430 \u0437\u0430 \u0434\u043E\u043F\u043E\u043C\u043E\u0433\
  \u043E\u044E \u043F\u0430\u043A\u0435\u0442\u0443 `temporary`."
title: "\u0421\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F \u0442\u0438\u043C\u0447\
  \u0430\u0441\u043E\u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0443"
weight: 21
---

## Як це зробити:
У Haskell, створити тимчасовий файл можна за допомогою пакету `temporary`. Спершу потрібно встановити пакет через `cabal` або `stack`.

```Haskell
import System.IO.Temp (withSystemTempFile)
import System.IO (hPutStrLn, hClose)

createTempFile = withSystemTempFile "mytemp.txt" $ \filePath handle -> do
  putStrLn $ "Використовуємо файл: " ++ filePath
  hPutStrLn handle "Тут може бути ваш текст"
  -- Файл буде видалено після завершення цього блоку
```

Запустивши `createTempFile`, отримаємо шлях до файлу на консолі і файл автоматично видалиться після виконання.

## Поглиблено:
Історично створення тимчасових файлів було важливим для операцій, які не могли виконуватися у пам'яті. В Haskell це також дозволяє уникнути конфліктів при паралельній роботі з файлами. Альтернатива – `temporary`-файл у пам'яті, але такий метод є обмежений розміром оперативної пам'яті. `withSystemTempFile` гарантує безпечне створення ім'я файлу, запобігає перетину шляхів і зіткненню імен.

## Більше інформації:
- [System.IO.Temp документація](https://hackage.haskell.org/package/temporary-1.3/docs/System-IO-Temp.html)
- [Стандартні шляхи у системах Unix-like](https://en.wikipedia.org/wiki/Unix_filesystem#Conventional_directory_layout)
