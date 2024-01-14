---
title:    "Fish Shell: Tworzenie pliku tekstowego"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego pisać pliki tekstowe?

Pisanie plików tekstowych jest bardzo przydatne w programowaniu w powłoce Fish Shell. Pozwala ono na przechowywanie kodu źródłowego w czytelnej formie, a także na łatwe udostępnianie i wersjonowanie kodu.

## Jak to zrobić?

Aby napisać plik tekstowy w Fish Shell, wystarczy wykorzystać polecenie `echo`. Przykładowo, jeśli chcemy stworzyć plik o nazwie `hello.txt` zawierający tekst "Witaj Świecie!", wykonujemy poniższą komendę:

```Fish Shell
echo "Witaj Świecie!" > hello.txt
```

Możemy także dopisywać do istniejącego pliku, korzystając z operatora `>>`. Na przykład, aby dodać wyraz "Dziękuję" do pliku `hello.txt`, używamy poniższej komendy:

```Fish Shell
echo "Dziękuję" >> hello.txt
```

## Deep Dive

Pamiętajmy, że polecenie `echo` wypisze podany tekst nie tylko do pliku, ale także na ekran konsoli. Jeśli chcemy zapisać tylko do pliku, możemy skorzystać z opcji `-n`, która wyłączy wypisywanie nowej linii:

```Fish Shell
echo -n "Hello" > hello.txt
```

W przypadku bardziej skomplikowanych plików tekstowych, możemy wykorzystać polecenie `cat`, które pozwala na konkatenację kilku plików tekstowych w jeden. Na przykład, jeśli mamy dwa pliki `hello.txt` i `world.txt`, możemy je połączyć w jeden za pomocą poniższej komendy:

```Fish Shell
cat hello.txt world.txt > hello_world.txt
```

## Zobacz także

- [Dokumentacja Fish Shell](https://fishshell.com/docs/current/)
- [Tutorial Fish Shell](https://fishshell.com/docs/current/tutorial.html)
- [Inne języki programowania](https://www.taniom.com/czym-sie-rozni-jezyk-programowania-od-jednego-judenka-programowania-do-drugiego/)