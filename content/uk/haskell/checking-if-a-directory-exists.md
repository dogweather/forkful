---
title:                "Haskell: Перевірка наявності каталогу"
programming_language: "Haskell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Чому

Перевірка існування каталогу є важливою задачею для розробників, оскільки це допомагає їм переконатися, що необхідний для роботи коду каталог існує та доступний.

## Як

```Haskell
import System.Directory

doesDirectoryExist :: FilePath -> IO Bool

main :: IO ()
main = do
    let directory = "~/Documents"
    exists <- doesDirectoryExist directory
    if exists
        then putStrLn $ directory ++ " існує!"
        else putStrLn "Каталог не знайдено."
```

В цьому прикладі ми імпортуємо модуль `System.Directory`, який містить функцію `doesDirectoryExist`, що приймає шлях до каталогу та повертає значення типу `IO Bool` - `True`, якщо каталог існує, та `False` - якщо каталог не знайдено. Ми також використовуємо функцію `putStrLn` для виведення повідомлення користувачу про результат перевірки існування каталогу.

## Глибше вдивимося

У функції `doesDirectoryExist` використовується функція `isDirectory`, яка перевіряє, чи існує файл за вказаним шляхом та чи є він каталогом. Це робиться шляхом перевірки додаткових деталей про файл, таких як його атрибути.

Під капотом функції `doesDirectoryExist` використовується системна виклик `access`, який перевіряє права доступу до файлу та його існування в системі. Це дозволяє глибше перевірити, чи існує каталог за вказаним шляхом.

## Дивіться також

- [Документація по модулю System.Directory](https://hackage.haskell.org/package/directory/docs/System-Directory.html)
- [Чому потрібно перевіряти існування каталогу у Haskell](https://stackoverflow.com/questions/44514889/check-if-directory-exists-in-haskell)