---
title:                "Fish Shell: Odczytywanie argumentów z wiersza poleceń"
programming_language: "Fish Shell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Dlaczego

Programowanie w powłoce przy użyciu komend wiersza poleceń jest bardzo przydatne i ważne dla każdego programisty. Pozwala ono na szybkie i efektywne wykonywanie różnych działań na plikach i folderach. W tym artykule dowiesz się jak czytać argumenty wiersza poleceń, co ułatwi Ci pracę z Fish Shell.

## Jak to zrobić

Pierwszą rzeczą, którą należy zrobić, aby czytać argumenty wiersza poleceń, jest użycie funkcji `argparse`, która jest wbudowana w Fish Shell. Korzystając z tej funkcji, możemy wczytać wszystkie parametry podane przez użytkownika w linii poleceń.

```Fish Shell
argparse ^znacznik^ --^opcja^ ^argument^
```

Używając powyższej komendy, Fish Shell będzie odczytywał wszystkie parametry, a także informować nas o błędach, jeśli użytkownik podał niewłaściwe argumenty.

```Fish Shell
argparse ^-f^ --^file^ ^plik.txt^
```

W tym przykładzie, za pomocą opcji `-f` lub `--file`, możemy określić nazwę pliku, który chcemy wczytać. W pierwszym przypadku używamy skróconej wersji, a w drugim pełnej nazwy. Następnie podajemy nazwę pliku `plik.txt` jako argument.

Możemy również użyć opcji bez argumentów, aby w prosty sposób włączyć lub wyłączyć daną funkcję.

## Deep Dive

Jeśli chcemy dokładniej zrozumieć, jak działa funkcja `argparse`, może przydać się nam wiedza o tzw. flagach. Flagi to opcje bez argumentów, którymi możemy włączyć lub wyłączyć daną funkcję. Są one poprzedzone znakiem `-` lub `--`, a w przypadku opcji z argumentami, po nich wstawiamy nazwę argumentu.

Możemy również określić domyślne wartości dla argumentów, jeśli użytkownik nie poda ich w linii poleceń. Robimy to za pomocą opcji `default`, np. `argparse --file ^plik.txt^^ -^f^ ^default "plik.txt"^`.

Pamiętaj również, że nie musimy używać `argparse` tylko w głównym pliku z kodem. Możemy stworzyć nowy plik `przyklad.fish` i wstawić tam nasz kod z `argparse`, a następnie wywołać go w głównym pliku, np. `source przyklad.fish`.

## Zobacz również
- [Dokumentacja Fish Shell](https://fishshell.com/docs/current/cmds/argparse.html)
- [Poradnik programowania w Fish Shell](https://www.sitepoint.com/fish-shell-one-configurable-shell/)
- [Porady i triki dla programistów używających Fish Shell](https://defkey.com/downloads/fish-shell)