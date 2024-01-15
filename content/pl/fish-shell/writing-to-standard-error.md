---
title:                "Pisanie do standardowego wyjścia błędu"
html_title:           "Fish Shell: Pisanie do standardowego wyjścia błędu"
simple_title:         "Pisanie do standardowego wyjścia błędu"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Dlaczego

Kodowanie w Fish Shell jest jednym z najbardziej popularnych sposobów na wykonywanie poleceń w terminalu. Jedną z ważnych umiejętności jest pisanie do standardowego błędu (stderr). W tym krótkim artykule dowiecie się, dlaczego warto nauczyć się tego podstawowego aspektu programowania w Fish Shell.

## Jak to zrobić

Pisanie do stderr jest wyjątkowo proste w Fish Shell. Wystarczy użyć specjalnego operatora `>&2` w linii kodu, aby wysłać dane do standardowego błędu. Poniżej przedstawiamy przykładowy kod, który wyświetla błąd w przypadku podania błędnych argumentów podczas uruchamiania skryptu:

```Fish Shell
if not test -f $argv[1]
    echo "Podany plik nie istnieje" >&2
    exit 1
end
```

W tym przykładzie, jeśli podany plik nie istnieje, zostanie wyświetlona wiadomość błędu na standardowym wyjściu błędu. Możesz również użyć `2>` aby przekierować wyjście błędu do pliku.

## Deep Dive

Większość programów i skryptów generuje wyjście błędu w celu poinformowania użytkownika o ewentualnych błędach lub problemach. Pisanie do standardowego błędu jest szczególnie przydatne w przypadku skryptów, gdy chcemy wyświetlić wiadomość błędu i przerwać wykonanie dalszej części kodu.

W Fish Shell, możemy również użyć funkcji `fish_add_error_handler` w celu definiowania własnych obsług błędów i wyświetlania spersonalizowanych wiadomości. Możesz się nauczyć więcej na temat tej funkcji poprzez wpisanie `help fish_add_error_handler` w terminalu.

## Zobacz także

- [Dokumentacja Fish Shell](https://fishshell.com/docs/current/index.html)
- [Oficjalny Poradnik Fish Shell](https://fishshell.com/docs/current/index.html#tut_friendly)
- [Poradnik na temat pisanie skryptów w Fish Shell](https://fishshell.com/docs/current/commands.html#commands-writingScripts)