---
title:                "Fish Shell: Czytanie argumentów z wiersza poleceń"
simple_title:         "Czytanie argumentów z wiersza poleceń"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli jesteś nowym programistą lub chcesz zmienić swoją technikę programowania, powinieneś zainteresować się komunikacją z wiersza poleceń. Pozwoli to na szybsze i bardziej skuteczne wykonywanie zadań, a także nauczy Cię bardziej efektywnego korzystania z systemu operacyjnego.

## Jak To Zrobić

Aby czytać argumenty z wiersza poleceń w Fish Shell, możesz użyć funkcji "arg". Przykładowy kod wygląda następująco:

```
Fish Shell: arg NAME
echo $NAME
```

Po uruchomieniu tego kodu ze zdefiniowanym argumentem, zobaczysz jego nazwę w konsoli. Przykładowe wyjście może wyglądać tak:

```
$ fish my_script.fish -n Lila
Lila
```

## Deep Dive

Po zrozumieniu podstawowej funkcji "arg", warto poznać dokładniejsze informacje na temat czytania argumentów z wiersza poleceń. W Fish Shell możesz użyć zmiennych specjalnych, takich jak $argv i $argc, aby uzyskać dostęp do wszystkich argumentów i ich liczby. Możesz także użyć pętli for w celu przetworzenia wszystkich argumentów za pomocą jednego kodu.

## Zobacz również

Jeśli chcesz dowiedzieć się więcej na temat programowania w Fish Shell, zapoznaj się z następującymi linkami:

- [Dokumentacja Fish Shell](https://fishshell.com/docs/current/)
- [Poradnik początkującego programisty Fish Shell](https://fishshell.com/docs/current/tutorial.html)
- [Przykładowy skrypt w Fish Shell](https://git.sr.ht/~oddmagne/scripts/tree/master/fish)