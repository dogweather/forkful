---
title:                "Створення тимчасового файлу"
html_title:           "Clojure: Створення тимчасового файлу"
simple_title:         "Створення тимчасового файлу"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Для чого
Створення тимчасового файлу може бути корисним для тих, хто працює з програмами, які потребують тимчасових даних. Це може бути використано для зберігання проміжних результатів обчислень, тимчасового зберігання вхідних даних або для тестування функціоналу.

## Як
Для створення тимчасового файлу використовується built-in функція `tmp-file`. Наприклад, якщо ми хочемо створити тимчасовий файл з назвою "temporary.txt", то можемо написати наступний код:
```Clojure
(def temp-file (tmp-file "temporary.txt"))
```
Цей код створить файл з назвою "temporary.txt", а його шлях буде збережений в змінній `temp-file`. Для запису даних в цей файл можна використати функцію `spit`, наприклад:
```Clojure
(spit temp-file "Hello, world")
```
Для отримання вмісту тимчасового файлу можна використати функцію `slurp`, наприклад:
```Clojure
(slurp temp-file) ; поверне "Hello, world"
```

## Deep Dive
Крім використання `tmp-file` для створення тимчасових файлів, існують також інші функції, які можуть бути корисними. Функція `with-tempfile` дозволяє створити тимчасовий файл та автоматично його видалити після виконання певних дій. Наприклад, якщо ми хочемо додати додаткову логіку для створеного тимчасового файла, можемо використати такий шаблон:
```Clojure
(with-tempfile [temp-file "temporary.txt"]
  (println "Дії з тимчасовим файлом виконуються тут")
  (spit temp-file "Додатковий вміст")
  (slurp temp-file)) ; поверне "Додатковий вміст"
```
Також можна вказати додаткові параметри для створення тимчасового файлу, наприклад, його розмір чи кодування.

## Дивіться також
- [Офіційна документація Clojure для функції `tmp-file`](https://clojuredocs.org/clojure.core/tmp-file)
- [Про керування тимчасовими файлами в Clojure](https://ucilnica.fri.uni-lj.si/pluginfile.php/100522/mod_resource/content/1/LevstikMiklavcic-Clojure-tmpfile.pdf)
- [Стаття про використання `with-tempfile` у Clojure](http://blog.timmattison.com/archives/2016/07/18/the-long-tail-of-clojure-how-to-use-with-tempfile/)