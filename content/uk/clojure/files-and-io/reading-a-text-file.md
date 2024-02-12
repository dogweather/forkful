---
title:                "Читання текстового файлу"
aliases: - /uk/clojure/reading-a-text-file.md
date:                  2024-01-20T17:54:28.507662-07:00
model:                 gpt-4-1106-preview
simple_title:         "Читання текстового файлу"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Що і Чому?
Читання текстового файлу — це процес отримання даних із файлу, збереженого на диску. Програмісти роблять це, щоб маніпулювати інформацією та інтегрувати дані у свої програми.

## Як це зробити:
```Clojure
;; Читання усього файлу як рядка
(slurp "шлях/до/файлу.txt")

;; Читання файла по рядках
(with-open [rdr (reader "шлях/до/файлу.txt")]
  (doseq [line (line-seq rdr)]
    (println line)))
```
Приклад вивода:
```
"Це перший рядок вашого текстового файлу."
"Це другий рядок, гарного дня!"
...
```

## Поглиблено:
Читання текстових файлів у Clojure не нове. Принципи схожі на інші мови програмування, але з ідіоматичним синтаксисом Clojure. Функція `slurp` проста для використання, коли файл не надто великий. Для великих файлів краще використовувати ліниве читання рядків через `line-seq`, щоб уникнути нестачі пам'яті.

Інші способи:
- Використання бібліотеки `clojure.java.io` для більш складних операцій (наприклад, з двійковими файлами).
- Лінива обробка даних з потоком (streams) і `with-open` дає змогу ефективно обробляти файли.

Імплементація:
Для читання файлів Clojure використовує Java Virtual Machine (JVM), а отже і Java API. Це дає стабільність та швидкодію.

## Дивіться також:
- [Clojure Documentation on clojure.java.io](https://clojure.github.io/clojure/clojure.java.io-api.html)
- [ClojureDocs Community-Powered Clojure Documentation and Examples](https://clojuredocs.org/)
- [Practical Clojure (Book)](http://www.pragprog.com/titles/shcloj/practical-clojure)
- [Clojure for the Brave and True (Online Book)](https://www.braveclojure.com/)
