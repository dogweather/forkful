---
title:                "Bash: Tworzenie pliku tymczasowego"
simple_title:         "Tworzenie pliku tymczasowego"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Tworzenie plików tymczasowych jest niezwykle ważne w programowaniu Bash. Są one niezbędne dla przechowywania chwilowych danych, dzięki czemu nie zaburzają one istniejących plików lub struktur katalogów. Tworzenie tymczasowych plików jest również niezbędne w tworzeniu skryptów, które wymagają korzystania z wielu plików tymczasowych jednocześnie.

## Jak to zrobić

Aby utworzyć plik tymczasowy w Bash, możemy skorzystać z komendy `mktemp`. Polecenie to generuje unikalny plik tymczasowy na podstawie określonego szablonu, który można dostosować. Przykładowy kod wyglądałby następująco:

```Bash
temp_file=$(mktemp prefix_XXXXXXXXXX)
echo "To jest przykładowe dane" > $temp_file
cat $temp_file
```

W powyższym kodzie używamy szablonu `prefix_XXXXXXXXXX`, który zostanie zastąpiony przez losowe 10 znaków. Następnie pobieramy nazwę wygenerowanego pliku tymczasowego do zmiennej `temp_file`, a następnie wyświetlamy zawartość pliku i czyszczymy go.

Warto także wspomnieć o opcji `-d` dla komendy `mktemp`, która pozwala na utworzenie tymczasowego katalogu zamiast pliku.

## Pogłębione informacje

Podczas tworzenia pliku tymczasowego, ważne jest, aby upewnić się, że nazwa pliku jest unikalna, aby uniknąć przypadkowego nadpisania istniejących plików. Dlatego też, warto dodać prefiks lub sufiks do nazwy pliku w szablonie. Możemy również użyć opcji `-u` w celu wyświetlenia tylko wygenerowanej nazwy pliku, bez jego utworzenia.

Dodatkowo, istnieje również możliwość ustawienia innego katalogu tymczasowego, w którym plik będzie tworzony, przy użyciu zmiennej środowiskowej `TMPDIR`.

## Zobacz także

- [Dokumentacja komendy mktemp](https://www.man7.org/linux/man-pages/man1/mktemp.1.html)
- [Inne sposoby tworzenia plików tymczasowych w Bash](https://www.shellhacks.com/create-temporary-file-one-line-bash-script/)
- [Poradnik tworzenia plików tymczasowych dla początkujących](https://www.ostechnix.com/create-temporary-files-shell-scripting/)