---
date: 2024-01-20 17:54:30.751018-07:00
description: "How to: (\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\
  \u0438:) \u0427\u0438\u0442\u0430\u0454\u043C\u043E \u0437 \u0444\u0430\u0439\u043B\
  \u0443 `example.txt`."
lastmod: '2024-04-05T21:53:50.140660-06:00'
model: gpt-4-1106-preview
summary: "(\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438:)\
  \ \u0427\u0438\u0442\u0430\u0454\u043C\u043E \u0437 \u0444\u0430\u0439\u043B\u0443\
  \ `example.txt`."
title: "\u0427\u0438\u0442\u0430\u043D\u043D\u044F \u0442\u0435\u043A\u0441\u0442\u043E\
  \u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0443"
weight: 22
---

## How to: (Як це зробити:)
Читаємо з файлу `example.txt`:

```Fish Shell
cat example.txt
```
Вивід:

```
Це зміст вашого файлу 
рядок за рядком.
```

## Deep Dive (Поглиблений Аналіз)
Читання файлів у командній оболонці Fish, як і в інших шелах, - базова функція. Раніше, в оболонках типу Bourne shell або Bash, використовувалися схожі команди. У Fish, `cat` безпосередньо виводить вміст файлу у термінал. Альтернативи під Fish - `less` для перегляду файлу посторінково і `grep` для пошуку. Працюючи з файлами, важливо розуміти переваги текстових кодувань (UTF-8 тощо) та кінцеві повороти рядків (LF чи CRLF).

## See Also (Див. Також)
- Офіційна документація Fish Shell: [https://fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)
- Перегляд файли з `less`: [https://fishshell.com/docs/current/commands.html#less](https://fishshell.com/docs/current/commands.html#less)
