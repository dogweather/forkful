---
date: 2024-01-20 17:41:59.426400-07:00
description: "Jak to zrobi\u0107: Fish Shell, kt\xF3ry zadebiutowa\u0142 w 2005 roku,\
  \ jest wygodnym interpreterem z automatycznym uzupe\u0142nianiem i kolorowym wy\u015B\
  wietlaniem,\u2026"
lastmod: '2024-04-05T21:53:37.250653-06:00'
model: gpt-4-1106-preview
summary: "Fish Shell, kt\xF3ry zadebiutowa\u0142 w 2005 roku, jest wygodnym interpreterem\
  \ z automatycznym uzupe\u0142nianiem i kolorowym wy\u015Bwietlaniem, skupiaj\u0105\
  cym si\u0119 na u\u017Cyteczno\u015Bci."
title: "Usuwanie znak\xF3w pasuj\u0105cych do wzorca"
weight: 5
---

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
