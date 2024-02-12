---
title:                "Перевірка наявності директорії"
date:                  2024-02-03T19:07:47.923251-07:00
model:                 gpt-4-0125-preview
simple_title:         "Перевірка наявності директорії"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що та чому?
Перевірка існування директорії є фундаментальною операцією в багатьох програмних завданнях, що дозволяє виконувати умовні дії залежно від наявності чи відсутності структур директорій. Це важливо для маніпуляцій з файлами, автоматизованих скриптів та під час початкової настройки програмного забезпечення, щоб переконатися, що необхідні директорії на місці, або щоб уникнути їх дублювання.

## Як це зробити:
Haskell за допомогою своєї базової бібліотеки пропонує прості способи перевірки на існування директорії, головним чином використовуючи модуль `System.Directory`. Давайте розглянемо базовий приклад:

```haskell
import System.Directory (doesDirectoryExist)

main :: IO ()
main = do
  let dirPath = "/path/to/your/directory"
  exists <- doesDirectoryExist dirPath
  putStrLn $ "Does the directory exist? " ++ show exists
```

Приклад виводу залежно від того, чи директорія існує:

```
Does the directory exist? True
```
Або:
```
Does the directory exist? False
```

Для більш складних сценаріїв або додаткової функціональності, ви можете розглянути популярну сторонню бібліотеку, таку як `filepath`, для обробки та маніпуляції шляхами файлів у більш абстрактний спосіб. Однак, для простої перевірки існування директорії, базова бібліотека `System.Directory` є достатньою та ефективною.

Пам'ятайте, робота з файловими системами може варіюватися на різних платформах, і підхід Haskell має на меті абстрагувати деякі з цих відмінностей. Завжди тестуйте свої операції з файлами на цільовій системі, щоб забезпечити очікувану поведінку.