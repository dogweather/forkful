---
title:                "Haskell: Перевірка наявності папки"
simple_title:         "Перевірка наявності папки"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Чому

Багато програматорів, особливо тих, хто працює з файловою системою, постійно стикаються з проблемою перевірки наявності директорії. Це важливо зробити перед спробою доступу до файлів або перед створенням нових. У цій статті я розкажу, як це зробити за допомогою мови програмування Haskell.

## Як зробити

Для перевірки наявності директорії спочатку потрібно імпортувати модуль `System.Directory`. Потім ми можемо використати функцію `doesDirectoryExist`, яка приймає шлях до директорії та повертає булеве значення `True`, якщо директорія існує, або `False`, якщо її немає.

```Haskell
import System.Directory

main = do
    let path = "./my_directory" -- змінна, що містить шлях до директорії
    exists <- doesDirectoryExist path -- перевіряємо наявність директорії
    if exists
        then putStrLn "Директорія існує."
        else putStrLn "Директорії не існує."
```

Ігноруйте цей код, якщо не знайомі з мовою Haskell, і просто виконайте наступні кроки:

1. Створіть новий проект Haskell за допомогою `stack new my_project`.
2. Додайте `System.Directory` в секцію `Build-depends` у файлі `my_project.cabal`.
3. Замініть вміст файлу `app/Main.hs` на коди з цього прикладу.
4. Відкрийте термінал в папці з проектом та виконайте `stack build` та `stack exec my_project`.

Якщо ви бачите виведення `Директорія існує.`, то це означає, що директорія `my_directory` існує, інакше буде виведено `Директорії не існує.`

## Глибше

Функція `doesDirectoryExist` використовує системний виклик `lstat` для перевірки, чи існує файл чи директорія за вказаним шляхом. Вона також перевіряє, чи не є файл посиланням або символьним посиланням. Якщо немає доступу до системних викликів, функція поверне `False` без виклику системних викликів.

## Дивись також

- [Haskell: Робота з файловою системою](https://wiki.haskell.org/Working_with_files)
- [Haskell System.Directory документація](https://hackage.haskell.org/package/directory-1.3.0.2/docs/System-Directory.html)