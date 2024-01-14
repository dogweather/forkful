---
title:    "Bash: Tworzenie pliku tekstowego"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Dlaczego

Cześć czytelniku! Jeśli jesteś tutaj, prawdopodobnie interesujesz się programowaniem w Bash lub po prostu chcesz dowiedzieć się więcej na ten temat. Dzisiejszy post będzie dotyczył tworzenia plików tekstowych za pomocą języka Bash. Poniżej dowiesz się dlaczego warto poznać tę umiejętność.

Tworzenie plików tekstowych za pomocą Bash jest bardzo użytecznym i praktycznym narzędziem dla programistów, a także dla osób, które chcą nauczyć się podstaw programowania. Jest to szybki i prosty sposób na zapisywanie informacji i różnych danych w jednym miejscu. Pliki tekstowe są również łatwo czytelne przez inne aplikacje, co czyni je uniwersalnym formatem przechowywania danych.

## Jak to zrobić

Teraz przejdziemy do praktyki. Najpierw otworzymy terminal i utworzymy nowy plik tekstowy za pomocą polecenia `touch`. Następnie użyjemy polecenia `echo` aby wpisać nasz tekst do pliku. Przykładowy kod wyglądałby tak:

```Bash
touch tekstowy_plik.txt    # utworzenie nowego pliku
echo "To jest nasz przykładowy tekstowy plik" > tekstowy_plik.txt   # wpisanie tekstu do pliku
```

Jeśli chcemy dodać więcej tekstu do pliku, możemy użyć operatora `>>` zamiast `>` w poleceniu `echo`:

```Bash
echo "Ten tekst zostanie dodany do istniejącego pliku" >> tekstowy_plik.txt
```

Aby wyświetlić zawartość pliku w terminalu, możemy użyć polecenia `cat`:

```Bash
cat tekstowy_plik.txt
```

Bash umożliwia również czytanie danych z pliku i przypisanie ich do zmiennych. Służy do tego polecenie `read`, przykładowy kod wyglądałby tak:

```Bash
read tekst < tekstowy_plik.txt    # przypisanie zawartości pliku do zmiennej tekst
echo $tekst   # wyświetlenie zawartości zmiennej
```

## Deep Dive

Jeśli jesteś trochę bardziej zaawansowany w programowaniu w Bash, możesz wykorzystać polecenie `printf` do formatowania tekstu w pliku. Przykładowy kod wyglądałby tak:

```Bash
printf "Ilość to %d, a nazwa to %s" 10 plik.txt    # sformatowanie tekstu i wpisanie do pliku
```

Możliwości związane z tworzeniem plików tekstowych w Bash są bardzo szerokie i mogą być wykorzystywane w różnych celach. Przydatnego narzędzia do tworzenia plików tekstowych z wykorzystaniem Bash możesz również używać do przetwarzania dużej ilości danych i automatyzacji swoich zadań.

## Zobacz również

- [10 przydatnych poleceń w Bash](https://linuxconfig.org/10-useful-bash-commands)
- [Bash Tutorial dla początkujących](https://www.tutorialspoint.com/unix_commands/bash.htm)
- [Oficjalna dokumentacja Bash](https://www.gnu.org/software/bash/manual/bash.html)