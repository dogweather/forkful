---
date: 2024-01-20 17:54:28.507662-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u0427\u0438\u0442\u0430\u043D\u043D\u044F \u0442\u0435\u043A\u0441\u0442\u043E\
  \u0432\u0438\u0445 \u0444\u0430\u0439\u043B\u0456\u0432 \u0443 Clojure \u043D\u0435\
  \ \u043D\u043E\u0432\u0435. \u041F\u0440\u0438\u043D\u0446\u0438\u043F\u0438 \u0441\
  \u0445\u043E\u0436\u0456 \u043D\u0430 \u0456\u043D\u0448\u0456 \u043C\u043E\u0432\
  \u0438 \u043F\u0440\u043E\u0433\u0440\u0430\u043C\u0443\u0432\u0430\u043D\u043D\u044F\
  , \u0430\u043B\u0435 \u0437 \u0456\u0434\u0456\u043E\u043C\u0430\u0442\u0438\u0447\
  \u043D\u0438\u043C \u0441\u0438\u043D\u0442\u0430\u043A\u0441\u0438\u0441\u043E\u043C\
  \ Clojure. \u0424\u0443\u043D\u043A\u0446\u0456\u044F\u2026"
lastmod: '2024-04-05T22:51:01.842226-06:00'
model: gpt-4-1106-preview
summary: "\u0427\u0438\u0442\u0430\u043D\u043D\u044F \u0442\u0435\u043A\u0441\u0442\
  \u043E\u0432\u0438\u0445 \u0444\u0430\u0439\u043B\u0456\u0432 \u0443 Clojure \u043D\
  \u0435 \u043D\u043E\u0432\u0435."
title: "\u0427\u0438\u0442\u0430\u043D\u043D\u044F \u0442\u0435\u043A\u0441\u0442\u043E\
  \u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0443"
weight: 22
---

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
