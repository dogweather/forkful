---
title:    "Bash: Tworzenie pliku tymczasowego"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/bash/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

Kodowanie tymczasowych plików w Bashu

## Dlaczego

Tworzenie tymczasowych plików jest nieodłączną częścią programowania w Bashu. Są one przydatne w wielu przypadkach, na przykład gdy potrzebujemy tymczasowej lokalizacji do przechowywania danych lub chcemy przetestować pewne funkcje naszego kodu bez wpływu na istniejące pliki.

## Jak

Kodowanie tymczasowego pliku w Bashu jest bardzo proste i wykorzystuje polecenie `mktemp`. W poniższych przykładach wykorzystamy ten sam scenariusz - stworzymy plik tymczasowy, zapiszemy w nim pewne dane, a następnie go usuniemy.

```Bash
# Tworzymy nowy tymczasowy plik
TEMP=$(mktemp)

# Zapisujemy dane do pliku
echo "To jest przykładowy tekst" > $TEMP

# Wyświetlamy zawartość pliku
cat $TEMP

# Usuwamy plik tymczasowy
rm $TEMP
```

Po wykonaniu powyższego kodu, widzimy w konsoli output tekstu "To jest przykładowy tekst", który został zapisany do tymczasowego pliku. Jest to bardzo prosta i użyteczna funkcjonalność, która może być również wykorzystana w bardziej zaawansowanych skryptach.

## Deep Dive

Polecenie `mktemp` jest bardzo potężnym narzędziem, ponieważ daje nam możliwość tworzenia niemalże niestandardowych nazw dla naszych tymczasowych plików. Przykładowo, jeśli chcemy stworzyć plik tymczasowy o nazwie "my_temp_file", możemy wykorzystać opcję `--suffix`:

```Bash
TEMP=$(mktemp --suffix=my_temp_file)
```

Dodatkowo, `mktemp` jest również w stanie utworzyć tymczasowy katalog poprzez użycie opcji `mkdtemp`.

## Zobacz również

- [Dokumentacja polecenia `mktemp` (po polsku)](https://www.linux.org.pl/Download/aplikacje/mktemp/)
- [Przykłady użycia polecenia `mktemp` (po angielsku)](https://www.howtogeek.com/627662/how-to-create-temporary-and-break-apart-files-in-bash/)
- [Poradnik tworzenia plików tymczasowych w Bashu (po polsku)](https://naukaprogramowania.pl/bash/temporary-files/)