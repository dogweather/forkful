---
title:                "Перевірка наявності каталогу"
html_title:           "Haskell: Перевірка наявності каталогу"
simple_title:         "Перевірка наявності каталогу"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Чому

Перевірка існування директорії - це важлива частина програмування, оскільки це дозволяє уникнути помилок і збільшити надійність програми. Це особливо важливо при роботі зі змінними шляхами до директорій.

## Як це зробити

```Haskell
import System.Directory (doesDirectoryExist)

main = do
  -- Перевіряємо чи існує директорія "example"
  dirExists <- doesDirectoryExist "example"
  if dirExists
    then putStrLn "Директорія example існує"
    else putStrLn "Директорія example не існує"
```

Вище наведено приклад використання функції `doesDirectoryExist`, яка перевіряє існування директорії за заданим шляхом. У цьому прикладі ми перевіряємо чи існує директорія "example" і виводимо відповідне повідомлення. Також можна використовувати цю функцію для роботи зі змінними шляхами, наприклад, `doesDirectoryExist path`.

## Глибокий занурення

При роботі з директоріями, варто пам'ятати, що функція `doesDirectoryExist` не переглядає повний шлях до директорії, а лише перевіряє чи існує вона за заданим шляхом. Також існує функція `doesFileExist`, яка перевіряє існування файлів.

## Див. також

- Документація по функції doesDirectoryExist: https://hackage.haskell.org/package/directory/docs/System-Directory.html#v:doesDirectoryExist
- Приклади використання System.Directory: https://hackage.haskell.org/package/directory/docs/System-Directory.html
- Інші корисні функції для роботи з директоріями: https://hackage.haskell.org/package/directory/docs/System-Directory.html#t:Directory