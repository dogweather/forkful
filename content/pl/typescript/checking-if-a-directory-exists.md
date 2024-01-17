---
title:                "Sprawdzanie istnienia katalogu"
html_title:           "TypeScript: Sprawdzanie istnienia katalogu"
simple_title:         "Sprawdzanie istnienia katalogu"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

Czy katalog istnieje? - Sprawdzamy to w TypeScript

## Co & Dlaczego?
Sprawdzenie, czy katalog istnieje, jest procesem, który pozwala programistom na ustalenie, czy określony katalog jest obecny w danym systemie plików. Jest to ważne, ponieważ umożliwia to programistom podejmowanie odpowiednich działań w zależności od tego, czy dany katalog istnieje lub nie.

## Jak to zrobić:
Wykorzystując wbudowane funkcje w TypeScript, można w prosty sposób sprawdzić, czy katalog istnieje w systemie plików. Przykładowy kod wyglądałby następująco:

```TypeScript
import fs from 'fs';

fs.existsSync('/sciezka/do/katalogu');
```

Jeśli katalog istnieje, funkcja zwróci wartość true, w przeciwnym razie zwróci false.

## Głębsza analiza:
Sprawdzanie, czy katalog istnieje, jest często wykorzystywane podczas pisania aplikacji webowych lub programów, które muszą operować na plikach. Jest to szczególnie przydatne w przypadku, gdy program musi najpierw sprawdzić, czy określony plik znajduje się w danym katalogu, zanim podejmie działania z nim związane.

Alternatywnym sposobem na sprawdzenie, czy katalog istnieje, jest użycie funkcji statystyki wraz z odpowiednim parametrem, który wskazuje, czy plik jest katalogiem lub nie. Jednakże, ten sposób wymaga więcej kodu i jest mniej czytelny.

Implementacja wbudowanej funkcji fs.existsSync() jest oparta na dostępie do systemowego API systemu plików. Oznacza to, że wynik może różnić się w zależności od systemu operacyjnego.

## Zobacz także:
- Dokumentacja fs.existsSync(): https://nodejs.org/api/fs.html#fs_fs_existssync_path
- Przykładowy kod: https://www.w3schools.com/nodejs/ref_fs.asp