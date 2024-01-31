---
title:                "Zapisywanie pliku tekstowego"
date:                  2024-01-19
html_title:           "Arduino: Zapisywanie pliku tekstowego"
simple_title:         "Zapisywanie pliku tekstowego"

category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (Co i dlaczego?)
Zapisywanie pliku tekstowego to proces tworzenia lub modyfikacji plików zawierających tekst. Programiści robią to, aby przechowywać dane, konfiguracje, skrypty czy dokumentować kod.

## How to: (Jak to zrobić:)
```Fish Shell
# Utworzenie nowego pliku tekstowego
echo "To jest przykładowy tekst" > plik.txt

# Dodanie tekstu do istniejącego pliku
echo "Kolejna linia tekstu" >> plik.txt

# Wyświetlenie zawartości pliku
cat plik.txt

# Sample output:
# To jest przykładowy tekst
# Kolejna linia tekstu
```

## Deep Dive (Dogłębna analiza)
W przeszłości, takie zadania często realizowane były za pomocą narzędzi takich jak `ed` czy `sed`. Fish Shell upraszcza działania za pomocą wbudowanych komend jak `echo` i przekierowań `>` oraz `>>`. Zapisywanie pliku tekstowego w Fish korzysta z Unixowych konwencji i może być używany do współpracy z narzędziami jak `grep`, `awk`, spoza powłoki.

## See Also (Zobacz także)
- Oficjalna dokumentacja Fish Shell: [https://fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)
- Przewodnik po przekierowaniach w Fish Shell: [https://fishshell.com/docs/current/tutorial.html#redirects](https://fishshell.com/docs/current/tutorial.html#redirects)
- Unix Shell Scripting Tutorial: [http://www.freeos.com/guides/lsst/](http://www.freeos.com/guides/lsst/)
