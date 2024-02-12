---
title:                "Usuwanie znaków pasujących do wzorca"
aliases:
- /pl/fish-shell/deleting-characters-matching-a-pattern/
date:                  2024-01-20T17:41:59.426400-07:00
model:                 gpt-4-1106-preview
simple_title:         "Usuwanie znaków pasujących do wzorca"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Usuwanie znaków pasujących do wzorca to sposób na pozbycie się niechcianych fragmentów tekstu — szybkie cięcie w danych. Programiści robią to, gdy chcą oczyścić dane wejściowe, uszczuplić logi lub szykować dane do dalszego przetwarzania.

## Jak to zrobić:
```
Fish Shell
# Usuń wszystkie wystąpienia litery 'a' z tekstu
echo "banana" | string replace -a "a" ""
# Wynik: bnn

# Usuń cyfry z ciągu znaków
echo "f1sh 1s c00l" | string replace -ar "[0-9]" ""
# Wynik: fsh s cl

# Usuń wszystko od pierwszego wystąpienia 't' do końca
echo "important text to remove" | string match -r -- ".*?(t.*)"
# Wynik: important 
```

## Wnikliwe spojrzenie:
Fish Shell, który zadebiutował w 2005 roku, jest wygodnym interpreterem z automatycznym uzupełnianiem i kolorowym wyświetlaniem, skupiającym się na użyteczności. Alternatywą jest użycie tradycyjnego Bash czy Zsh z `sed` lub `awk`, ale Fish z jego `string` jest prostrzy w obsłudze. Podczas usuwania znaków, `string replace` oferuje elastyczność dzięki użyciu regexów. 

## Zobacz również:
- Oficjalna dokumentacja `string`: https://fishshell.com/docs/current/cmds/string.html
- Tutorial regex w Fish: https://fishshell.com/docs/current/tutorial.html#tut_regexes
- Porównanie powłok: https://fishshell.com/docs/current/design.html
