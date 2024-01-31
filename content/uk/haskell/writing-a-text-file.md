---
title:                "Створення текстового файлу"
date:                  2024-01-19
html_title:           "Arduino: Створення текстового файлу"
simple_title:         "Створення текстового файлу"

category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Що це та навіщо?
Запис текстового файлу - це процес збереження даних у файл на диску. Програмісти це роблять для зберігання результатів, налаштувань, логів виконання програм.

## Як це робити:

```Haskell
-- Запишемо рядок у файл
import System.IO

main :: IO ()
main = do
    let str = "Вітаємо у Haskell!"
    writeFile "example.txt" str
```

Після запуску файла `example.txt` міститиме текст:
```
Вітаємо у Haskell!
```

## Поглиблений огляд:
Історично, Haskell використовує lazy IO для роботи з файлами, але це може призвести до неочікуваних помилок. Тому частіше застосовують строгий ввід-вивід через `Data.ByteString` або `Data.Text`. Прямий запис файлу ефективний для одноразового запису, але для роботи з файлами потоково, рекомендуємо використовувати абстракції, такі як `hPutStrLn`, що працюють з `Handle`.

## Дивіться також:
- [Пакет `bytestring` для оптимізованої роботи з байтами](https://hackage.haskell.org/package/bytestring)
- [Пакет `text` для роботи з текстом](https://hackage.haskell.org/package/text)
