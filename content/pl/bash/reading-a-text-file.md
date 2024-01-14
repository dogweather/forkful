---
title:                "Bash: Czytanie pliku tekstowego"
simple_title:         "Czytanie pliku tekstowego"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Często musimy przeprowadzić analizę tekstu zapisanego w formie pliku. Może to być na przykład przetwarzanie danych z logów lub parsowanie informacji z plików konfiguracyjnych. W takich przypadkach warto znać podstawy czytania plików tekstowych za pomocą Bash.

## Jak to zrobić

Pierwszym krokiem jest otwarcie pliku do odczytu za pomocą polecenia `cat`:

```Bash
cat plik.txt
```

Następnie, jeśli chcemy wyświetlić tylko określoną ilość linii, możemy zastosować opcję `-n`:

```Bash
cat -n plik.txt
```

Jeśli natomiast chcemy przeszukać zawartość pliku w poszukiwaniu określonego ciągu znaków, użyjemy polecenia `grep`:

```Bash
grep "słowo" plik.txt
```

Możemy także zapisywać wyniki przetwarzania do nowego pliku, używając operatora `>`:

```Bash
cat plik.txt > nowy_plik.txt
```

## Mocniej w temat

W Bash mamy wiele opcji do przetwarzania tekstu zapisanego w pliku. Możemy na przykład sortować dane za pomocą `sort`, usunąć duplikaty za pomocą `uniq` lub zliczyć ilość linii korzystając z `wc`. Warto także zapoznać się z poleceniem `sed` pozwalającym na edycję tekstu oraz `awk` do przetwarzania plików w formacie CSV.

## Zobacz także

- [Podstawowe polecenia Bash](https://www.thegeekstuff.com/2010/06/bash-shell-builtin-functions/)
- [Przetwarzanie tekstu w Bash](https://www.tecmint.com/13-basic-cat-command-examples-in-linux/)
- [Rozwiązania problemów z plikami tekstowymi w Bash](https://www.shellhacks.com/bash-read-file-read-lines/)