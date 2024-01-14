---
title:                "Bash: Wyszukiwanie i zamiana tekstu"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Dlaczego

Jedną z najważniejszych umiejętności każdego programisty jest umiejętność obsługi tekstu. Bez względu na to, czy piszesz mały skrypt czy duży projekt, prawdopodobnie będziesz musiał przeprowadzić przeszukiwanie i zamianę tekstu w swoim kodzie. W tym wpisie dowiesz się, jak to zrobić w Bashu, języku programowania, który jest wykorzystywany przez wielu programistów i administratów systemów.

## Jak to zrobić

Przeszukiwanie i zamiana tekstu w Bashu jest łatwe i wygodne dzięki używaniu wbudowanych poleceń. Jedną z najczęściej wykorzystywanych komend jest `sed`, która służy do przetwarzania tekstu. Aby przeprowadzić prostą zamianę tekstu, możesz użyć polecenia `sed 's/tekst_do_znalezienia/nowy_tekst/' plik`, gdzie `tekst_do_znalezienia` to wyróżniony tekst, który chcesz zmienić, a `nowy_tekst` to jego zamieniona wersja. Możesz również użyć opcji `i` po poleceniu `sed` aby dokonać zmian bezpośrednio w pliku, zamiast wyświetlać wynik na ekranie. Przykładowy kod wyglądałby tak:

```Bash
sed -i 's/tekst_do_znalezienia/nowy_tekst/' plik
```

Jeśli chcesz przeprowadzić zmianę tylko w niektórych wierszach, możesz użyć opcji `r`, aby podać zakres wierszy, na którym ma zostać wykonane polecenie. Przykładowo, `sed -i '10,20s/tekst_do_znalezienia/nowy_tekst/' plik` zmieni tylko tekst w wierszach od 10 do 20.

Inną użyteczną komendą jest `tr`, która służy do tłumaczenia lub usuwania znaków. Aby przeprowadzić prostą zamianę liter, możesz użyć polecenia `tr 'ABCD' 'ZYXW' < plik`, gdzie `ABCD` to litery, które chcesz zamienić, a `ZYXW` to ich zamienione odpowiedniki. Możesz również użyć opcji `d` po poleceniu `tr` aby usunąć określone znaki. Przykładowy kod wyglądałby tak:

```Bash
tr 'abc' 'xyz' < plik
```

Bash oferuje również możliwość użycia wyrażeń regularnych przez dodanie opcji `E` po poleceniu `sed` lub `tr`.

## Deep Dive

W Bashu istnieje wiele innych komend i opcji, które umożliwiają zaawansowane przeszukiwanie i zamianę tekstu. Możesz również wykorzystać pętle i warunki do przetwarzania większych plików tekstowych lub wielu plików jednocześnie. Przydatne są także takie komendy jak `awk` czy `grep`, które dają jeszcze większe możliwości manipulacji tekstem.

## Zobacz również

Jeśli chcesz dowiedzieć się więcej o przeszukiwaniu i zamianie tekstu w Bashu, polecam zapoznać się z poniższymi linkami:

- [Dokumentacja sed](https://www.gnu.org/software/sed/manual/sed.html)
- [Wprowadzenie do przetwarzania tekstu w Bashu](https://www.howtogeek.com/430730/the-essential-guide-to-performing-text-manipulation-in-linux/)
- [Wyrażenia regularne w Bashu](https://www.linuxjournal.com/content/bash-regexps-beginners)
- [Manipulacja tekstem przy użyciu awk i sed](https://www.thegeekstuff.com/2010/06/awk-sed-part-1-strings-and