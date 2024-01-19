---
title:                "Konwersja daty na ciąg znaków"
html_title:           "Clojure: Konwersja daty na ciąg znaków"
simple_title:         "Konwersja daty na ciąg znaków"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Konwersja daty na ciąg znaków umożliwia przedstawienie daty w czytelnej formie. Programiści robią to, aby ułatwić interpretację daty przez użytkowników lub zapisanie jej do pliku.

## Jak to zrobić:

Poniższy kod Fish Shell pokazuje, jak przekształcić datę na ciąg znaków w standardowym formacie YYYY-MM-DD:

```Fish Shell
set data (date -u +"%Y-%m-%d")
echo $data
```

Kiedy go uruchomisz, zobaczysz coś takiego:

```Fish Shell
2022-03-25
```

Możesz także dostosować format, na przykład pokazać tylko rok i dzień:

```Fish Shell
set data (date -u +"%Y-%d")
echo $data
```

Da to wynik:

```Fish Shell
2022-25
```

## Pogłębione informacje:

Zasoby systemu Unix zapewniają funkcję `date`, która po raz pierwszy pojawiła się w systemie Time-Sharing System (TSS/8) w 1970 roku. To właśnie ona pozwala nam na manipulację datami w Fish Shell.

Alternatywnie, możesz użyć innych języków, takich jak Python czy JavaScript, aby przekształcić datę na ciąg znaków, jednak Fish Shell zapewnia prostszy sposób, bez potrzeby dodawania dodatkowych bibliotek.

Dużą zaletą jest możliwość dowolnego formatowania daty. Możemy ustawić dowolny układ rok, miesiąc, dzień dzięki różnym opcjom, które podajemy jako argumenty do funkcji `date`.

## Inne materiały:

Jeśli chcesz dowiedzieć się więcej o Fish Shell i manipulacji datami, oto kilka linków do powiązanych źródeł:

1. [Dokumentacja Fish Shell](https://fishshell.com/docs/)
2. [Artykuł na temat formatowania dat w Unix](https://www.geekhideout.com/date.shtml)
3. [Kurs online o Fish Shell](https://www.learnshell.org/)
4. [Dyskusja na StackExchange na temat konwersji daty na ciąg znaków](https://unix.stackexchange.com/questions/4671/date-command-conversion-to-string)