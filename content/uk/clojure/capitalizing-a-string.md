---
title:                "Clojure: Перетворення рядка на режим прописних літер"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Чому

Є багато ситуацій, коли потрібно змінити форматування рядка, особливо коли працюєш з великими наборами даних або виконуєш обчислення. Капіталізація рядка дозволяє перетворити першу літеру кожного слова великою, що допомагає полегшити читання та розуміння тексту.

## Як

```Clojure
(defn capitalize-string [string]
  (clojure.string/capitalize string))
```

В даному прикладі ми визначаємо функцію "capitalize-string", яка приймає параметр "string" та використовує вбудовану функцію clojure.string/capitalize для капіталізації рядка. Давайте спробуємо ввести рядок "Привіт, світ!" та подивимося на результат виконання функції.

```Clojure
(capitalize-string "Привіт, світ!")
```

Результат:

```Clojure
"Привіт, Світ!"
```

Окрім використання вбудованої функції, є й інші способи капіталізації рядка в Clojure. Наприклад, за допомогою функції "upper-case", яка змінює всі літери рядка на заголовні.

```Clojure
(upper-case "Привіт, світ!")
```

Результат:

```Clojure
"ПРИВІТ, СВІТ!"
```

## Deep Dive

У Clojure, рядки є не змінними, тобто змінити існуючий рядок неможливо, але можна створити новий рядок з потрібним форматуванням. Це робить код більш безпечним та покращує швидкодію програми.

Також, варто зауважити, що навіть якщо рядок містить кириличні символи, функція "capitalize" буде коректно працювати, оскільки вона використовує міжнародні правила капіталізації.

## Дивись також

- Документація по функції capitalize Clojure: https://clojuredocs.org/clojure.string/capitalize
- Стаття про роботу з рядками у Clojure: https://medium.com/@yagudaev/working-with-strings-in-clojure-5137ea08ad84
- Український спільноти Clojure: https://clojure.org/community/developer_groups#Ukraine