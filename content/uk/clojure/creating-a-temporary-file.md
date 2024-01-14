---
title:                "Clojure: Створення тимчасового файлу."
simple_title:         "Створення тимчасового файлу."
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Для чого: 

Створення тимчасового файлу є незамінною частиною багатьох програм, особливо тих, які використовують зовнішні ресурси або потребують збереження тимчасових даних. Це також може бути корисно при тестуванні програм або відлагодженні коду. 

## Як це зробити: 

```Clojure 
(require '[clojure.java.io :as io]) 

(let [tmp-file (io/file "tmp/example.txt")] ; створення об'єкту файлу 
  (io/make-parents tmp-file) ; створення батьківських папок у потрібному шляху 
  (with-open [f (io/writer tmp-file)] ; відкриття з'єднання з файлом 
    (.write f "Hello, World!") ; запис даних у файл 
    (.flush f))) ; очищення буферу із записаними даними 
``` 

Результат: 
```
Hello, World! 
``` 

## Огляд: 

Створення тимчасового файлу в Clojure досить просте завдання. Ми використали функцію `io/file` для створення об'єкту файлу за вказаним шляхом. Потім ми використали функцію `io/make-parents`, яка автоматично створить всі необхідні батьківські папки. Далі, ми відкрили з'єднання з файлом за допомогою `with-open` та записали бажаний текст у файл. Не забувайте викликати `flush`, щоб зберегти записані дані. 

# Дивіться також: 

- [Документація з Java I/O для Clojure](https://clojuredocs.org/clojure.java.io)
- [Рекомендована практика щодо користування зовнішніми ресурсами в програмах на Clojure](https://www.lucidchart.com/techblog/2017/10/03/clojure-best-practices-for-working-with-external-resources)