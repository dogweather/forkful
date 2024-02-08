---
title:                "Sprawdzanie, czy katalog istnieje"
aliases:
- pl/fish-shell/checking-if-a-directory-exists.md
date:                  2024-02-03T19:07:20.328046-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sprawdzanie, czy katalog istnieje"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?
Sprawdzanie, czy katalog istnieje w powłoce Fish, pozwala skryptom na podejmowanie decyzji w oparciu o obecność lub brak struktur katalogów, umożliwiając zadania takie jak warunkowe operacje na plikach, logowanie czy konfiguracja środowiska. Ta technika jest kluczowa dla pisania solidnych skryptów, które w przewidywalny sposób wchodzą w interakcję z systemem plików.

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
