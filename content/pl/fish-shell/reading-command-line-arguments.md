---
title:                "Odczytywanie argumentów wiersza poleceń"
html_title:           "Fish Shell: Odczytywanie argumentów wiersza poleceń"
simple_title:         "Odczytywanie argumentów wiersza poleceń"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Reading command line arguments jest procesem, w którym program odczytuje i wykorzystuje informacje podane w linii poleceń przez użytkownika. Jest to powszechne wśród programistów, ponieważ pozwala na dostosowanie działania programu do konkretnych potrzeb użytkownika.

## Jak to zrobić:

Fish Shell posiada wbudowane funkcje, które pozwalają na obsługę argumentów linii poleceń. Można to zrobić za pomocą pętli `for` lub funkcji `argv`. Przykłady kodu poniżej pokazują, jak można wykorzystać te funkcje w celu odczytania argumentów linii poleceń:

```Fish Shell 
for i in (seq (count $argv))
	echo "Argument nr. $i to $argv[$i]"
end
```

Przy użyciu pętli `for`, możemy przejść przez wszystkie argumenty linii poleceń i wyświetlić ich wartość. Natomiast przy użyciu funkcji `argv`, możemy odwoływać się bezpośrednio do konkretnego argumentu, podając jego numer w nawiasach kwadratowych.

Aby uruchomić ten kod, wystarczy zapisać go jako plik `arguments.fish` i uruchomić w terminalu komendą `fish arguments.fish argument1 argument2 argument3`. W efekcie powinniśmy otrzymać następujący output:

``` 
Argument nr. 1 to argument1
Argument nr. 2 to argument2
Argument nr. 3 to argument3
```

## Głębszy zanurzenie:

Odczytywanie argumentów linii poleceń jest ważnym elementem programowania i wykorzystywane jest w wielu językach i powłokach, nie tylko w Fish Shell. Podobną funkcjonalność zapewniają na przykład języki Python czy Bash.

W Fish Shell można również wykorzystać specjalne zmienne, takie jak `$fish_command`, które pozwalają na wykonywanie różnych akcji w zależności od użytej komendy. Możliwe jest również ustawianie opcji za pomocą flag, na przykład `--verbose` czy `--help`. Więcej informacji na ten temat można znaleźć w dokumentacji Fish Shell.

## Zobacz też:

Dokumentacja Fish Shell: https://fishshell.com/docs/current/

Przewodnik po argumentach linii poleceń: https://www.tldp.org/LDP/abs/html/othertypesv.html