---
title:    "Haskell: Створення тимчасового файлу"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

##Чому

Створення тимчасових файлів - це важлива частина програмування, особливо в мові Haskell. Є багато ситуацій, коли нам потрібно тимчасово зберігати дані або виконувати обчислення, і створення тимчасових файлів є надійним і ефективним способом цього зробити.

##Як

Створення тимчасових файлів в мові Haskell дуже просте за допомогою функції `withTempFile` і `writeFile`:

```Haskell
import System.IO.Temp (withTempFile)
import System.IO (writeFile)

withTempFile "temp.txt" $ \path handle -> do
    writeFile path "Привіт, Україно!"
    hClose handle
```

В цьому прикладі ми імпортуємо модулі `System.IO.Temp` і `System.IO` для роботи з тимчасовими файлами. Функція `withTempFile` приймає шаблон назви файлу і функцію, яка отримує шлях до створеного тимчасового файлу і обробляє його. У нашому випадку ми просто записуємо у файл рядок "Привіт, Україно!" за допомогою функції `writeFile`, а потім закриваємо файл за допомогою `hClose`.

В результаті ви отримаєте тимчасовий файл з назвою "temp.txt", який містить рядок "Привіт, Україно!".

##Глибоке дослідження

Функція `withTempFile` є лише одним з багатьох способів створення тимчасових файлів в мові Haskell. Існують також функції `openTempFile` і `createTempFile`, які дають більше контролю над процесом створення файлу.

Крім того, існує можливість використання бібліотеки `tempfile`, яка надає більше функцій для роботи з тимчасовими файлами, таких як генерація унікальної назви файлу або видалення тимчасового файлу після закінчення роботи з ним.

Нехай створення тимчасових файлів у мові Haskell стане для вас простою задачею завдяки цим корисним інструментам.

##Дивіться також

- [Документація по `System.IO.Temp`](https://hackage.haskell.org/package/temporary-1.3.0.0/docs/System-IO-Temp.html)
- [Документація по `tempfile`](https://hackage.haskell.org/package/tempfile-1.2.0.1/docs/System-IO-Temp.html)
- [Приклади використання тимчасових файлів в Haskell](https://www.fpcomplete.com/blog/2017/07/working-with-temporary-files-in-haskell/)