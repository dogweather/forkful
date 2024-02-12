---
title:                "Створення тимчасового файлу"
aliases:
- /uk/haskell/creating-a-temporary-file.md
date:                  2024-01-20T17:40:48.071109-07:00
model:                 gpt-4-1106-preview
simple_title:         "Створення тимчасового файлу"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Що це та Навіщо?
Створення тимчасового файлу - це спосіб маніпуляції даними, які потребують короткочасного зберігання. Програмісти використовують такі файли для забезпечення безпеки, уникнення конфліктів даних чи зменшення завантаження на оперативну пам'ять.

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
