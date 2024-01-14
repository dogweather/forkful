---
title:    "Haskell: Перевірка існування директорії."
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Чому

У цій статті ми дізнаємося, як перевірити існування директорії в мові програмування Haskell. Це корисна навичка, яка допоможе вам створювати додатки, що працюють з файловою системою.

## Як це зробити

```Haskell
import System.Directory

-- Перевіряємо, чи існує директорія
doesDirectoryExist :: FilePath -> IO Bool

-- Приклад використання
main = do
    -- Вказуємо шлях до потрібної директорії
    let dirPath = "C:/Users/username/Documents"
    -- Перевіряємо, чи існує директорія
    dirExist <- doesDirectoryExist dirPath
    -- Виводимо результат перевірки
    if dirExist
        then putStrLn "Директорія існує"
        else putStrLn "Директорія не існує"
```

В результаті ви отримаєте вивід "Директорія існує", якщо вказана директорія існує, або "Директорія не існує", якщо вона не існує.

## Глибинна аналітика

При використанні функції `doesDirectoryExist`, слід звернути увагу на те, що вона перевіряє наявність самої директорії, а не шляху до неї. Також варто пам'ятати, що ця функція використовується для взаємодії з файловою системою, тому перед її використанням слід переконатися, що ви маєте необхідні дозволи на зміни в директорії.

## Дивіться також

- [Офіційна документація System.Directory](http://hackage.haskell.org/package/directory/docs/System-Directory.html)
- [Стаття про роботу з файлами в Haskell](https://www.fpcomplete.com/haskell/tutorial/haskell-files/)
- [Стаття про роботу з файловою системою у мові Haskell](https://www.tutorialspoint.com/haskell/haskell_files_io.htm)