---
title:                "Створення тимчасового файлу"
html_title:           "C: Створення тимчасового файлу"
simple_title:         "Створення тимчасового файлу"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Що це таке і навіщо це потрібно?
Створення тимчасового файлу - це процес створення файлу, який використовується протягом короткого періоду часу для зберігання інформації в інформаційній системі. Програмісти роблять це для обміну даними між процесами або для відновлення інформації після аварійного завершення програми.

## Як це робиться:
Створимо тимчасовий файл в Haskell за допомогою бібліотеки `System.IO.Temp`.

```Haskell
import System.IO.Temp

main = withSystemTempFile "temp.txt" $ \path handle -> do
    hPutStrLn handle "Some test content"
    hClose handle
    content <- readFile path
    putStrLn content
```
При цьому ви створюєте тимчасовий файл з іменем "temp.txt", записуєте в нього деякий контент і закриваєте. Потім ви знову відкриваєте файл, читаєте його та виводите контент.
## Занурення у глибину:
Створення тимчасових файлів - це старий підхід, який використовується в багатьох операційних системах. Виконати це можна і інакше, наприклад, використовуючи пам'ять (RAM) замість диску. Однак, цей метод може спричинити проблеми з доступом до пам'яті, особливо при роботі з великими даними.

## Див. також:
Посібник Haskell для роботи з файлами: https://wiki.haskell.org/Introduction_to_IO
Бібліотека System.IO.Temp: https://hackage.haskell.org/package/temporary-1.3/docs/System-IO-Temp.html