---
title:                "Виконання текстового файлу"
html_title:           "Clojure: Виконання текстового файлу"
simple_title:         "Виконання текстового файлу"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Чому

В сьогоднішній цифровий вік, написання текстових файлів є важливою навичкою для будь-якого програміста. Вони дозволяють зберігати, обмінюватися та обробляти дані, що є необхідним для багатьох проектів.

## Як

Написання текстових файлів у Clojure дуже просте. Потрібно використовувати функцію `with-open` для створення з'єднання з файлом, а потім використовувати функцію `println` для додавання даних до файлу. Наприклад:

```Clojure
(with-open [file (clojure.java.io/writer "file.txt")]
  (println "Це текстовий файл.")
  (println "Це написано у Clojure."))
```

Результат:

```
Це текстовий файл.
Це написано у Clojure.
```

Можна також використовувати функцію `clojure.pprint/write` для структурированого виведення даних до файлу. Наприклад:

```Clojure
(with-open [file (clojure.java.io/writer "data.txt")]
  (clojure.pprint/write {"key1" "value1" "key2" "value2"} file))
```

Результат:

```
{:key1 "value1", :key2 "value2"}
```

Також можна використовувати функцію `spit` для швидкого запису даних до файлу. Наприклад:

```Clojure
(spit "info.txt" "Це ще один текстовий файл, написаний у Clojure.")
```

Результат:

```
Це ще один текстовий файл, написаний у Clojure.
```

## Деталі

Технічно, усі дані у Clojure є об'єктами, які можуть бути перетворені у рядки. Тому, любі дані можуть бути записані у текстовий файл. Також, важливо використовувати функцію `with-open` для закриття з'єднання з файлом після того, як ви закінчили працювати з ним.

## Дивіться також

- [Документація Clojure про роботу з файлами](https://clojure.org/guides/io)
- [Відео з написанням текстових файлів у Clojure](https://www.youtube.com/watch?v=Y3nP2dz91PQ)
- [Стаття про використання `with-open` для читання та запису файлів у Clojure](https://www.baeldung.com/java-read-write-from-file#clojure)