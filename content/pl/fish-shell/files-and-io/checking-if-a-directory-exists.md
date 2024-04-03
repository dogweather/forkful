---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:20.328046-07:00
description: "Jak to zrobi\u0107: Pow\u0142oka Fish u\u017Cywa komendy `test` do sprawdzania\
  \ typ\xF3w plik\xF3w i ich cech, w tym tego, czy cel jest katalogiem. Oto podstawowy\
  \ wzorzec\u2026"
lastmod: '2024-03-13T22:44:35.856171-06:00'
model: gpt-4-0125-preview
summary: "Pow\u0142oka Fish u\u017Cywa komendy `test` do sprawdzania typ\xF3w plik\xF3\
  w i ich cech, w tym tego, czy cel jest katalogiem."
title: Sprawdzanie, czy katalog istnieje
weight: 20
---

## Jak to zrobić:
Powłoka Fish używa komendy `test` do sprawdzania typów plików i ich cech, w tym tego, czy cel jest katalogiem. Oto podstawowy wzorzec sprawdzania, czy katalog istnieje:

```fish
if test -d /path/to/dir
    echo "Katalog istnieje"
else
    echo "Katalog nie istnieje"
end
```
Przykładowe wyjście:
```
Katalog istnieje
```

Dla bardziej płynnych operacji na plikach i katalogach, można sięgnąć po zewnętrzne narzędzia takie jak `fd`, chociaż jest ono częściej używane do znajdowania plików i katalogów, niż tylko do sprawdzania ich istnienia. Jednak połączenie go ze skryptowaniem w Fish może przynieść pożyteczne rezultaty:

```fish
set dir "/path/to/search"
if fd . $dir --type directory --max-depth 1 | grep -q $dir
    echo "Katalog istnieje"
else
    echo "Katalog nie istnieje"
end
```

Ten przykład z użyciem `fd` wyszukuje katalog na określonej głębokości, a `grep` sprawdza zgodność, czyniąc go wszechstronnym dla zniuansowanych sprawdzeń. Jednakże, dla bezpośredniego celu sprawdzania istnienia, trzymanie się wbudowanej w Fish komendy `test` jest zarówno efektywne, jak i proste.
