---
title:                "Читання текстового файлу"
date:                  2024-01-20T17:54:30.751018-07:00
model:                 gpt-4-1106-preview
simple_title:         "Читання текстового файлу"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/fish-shell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (Що та Чому?)
Читання текстового файлу - це процес здобуття даних з файлу для їхньої обробки. Програмісти це роблять для аналізу, обробки інформації чи конфігурації програм.

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
