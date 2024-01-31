---
title:                "Написання тестів"
date:                  2024-01-19
html_title:           "Arduino: Написання тестів"
simple_title:         "Написання тестів"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/fish-shell/writing-tests.md"
---

{{< edit_this_page >}}

## Що це & Навіщо?
Тести дозволяють перевірити, чи правильно працює код. Програмісти пишуть тести, щоб уникнути помилок та підвищити якість програмного забезпечення.

## Як це зробити:
```Fish Shell
function test_greeting
    set result (greeting)
    if test "$result" = "Привіт, світ!"
        echo "Тест пройшов: $result"
    else
        echo "Тест не пройшов: $result"
    end
end

test_greeting
```
Вивід:
```Fish Shell
Тест пройшов: Привіт, світ!
```

## Поглиблено:
Test-driven development (TDD) – методика розробки ПЗ, де спочатку пишуть тести. ShellCheck та Fishtape – інструменти для тестування в shell. У Fish немає вбудованих функцій для тестування, але можна використати зовнішні інструменти або писати прості перевірки власноруч.

## Дивіться також:
- [Fish Shell Documentation](https://fishshell.com/docs/current/index.html)
- [Fishtape for Fish Shell tests](https://github.com/jorgebucaran/fishtape)
- [ShellCheck – аналізатор скриптів Shell](https://www.shellcheck.net/)
