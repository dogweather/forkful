---
title:                "Interpolowanie ciągu znaków."
html_title:           "Bash: Interpolowanie ciągu znaków."
simple_title:         "Interpolowanie ciągu znaków."
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/interpolating-a-string.md"
---

{{< edit_this_page >}}

Cześć czytelnicy! Dzisiaj porozmawiamy o interpolarowaniu stringów w języku Bash. Może brzmi to skomplikowanie, ale w rzeczywistości jest to bardzo przydatna umiejętność dla programistów. Dlatego też warto poświęcić chwilę czasu, aby poznać podstawy tego zagadnienia.

## Co to jest i dlaczego to robimy?
Interpolowanie stringów to po prostu wstawianie wartości zmiennych lub wyrażeń wewnątrz innych ciągów znaków. Programiści używają tego, aby dynamicznie tworzyć tekst, który zawiera informacje zmienne. Jest to szczególnie przydatne, gdy chcemy wyświetlić jakieś dane lub komunikaty użytkownikowi w naszym programie.

## Jak to zrobić?
Do interpolarowania stringów w Bash wykorzystujemy specjalny operator ```$```, którego używamy w następujący sposób:

```
imie="Jan"
echo "Witaj, $imie!"
```
To spowoduje wyświetlenie na ekranie napisu "Witaj, Jan!". Jak widzimy, nazwa zmiennej jest umieszczana wewnątrz cudzysłowu i poprzedzana znakiem ```$```. Jest to sygnał dla interpretera, aby zastąpić tę część ciągu wartością przechowywaną w danej zmiennej.

Możemy również wykonywać operacje matematyczne lub wyrażenia logiczne wewnątrz takiego ciągu znaków. Na przykład:

```
a=5
b=2
echo "Wynik dodawania wynosi: $(($a + $b))"
```
Powyższy kod spowoduje, że na ekranie pojawi się napis "Wynik dodawania wynosi: 7".

Możliwości jest wiele, więc warto poeksperymentować i odkryć, jakie ciekawe "szalone" rzeczy możemy zrobić, interpolując stringi w Bash.

## Rzecz w głębi
Historia interpolarowania stringów sięga lat 60., kiedy to miało swoją początkową formę w językach takich jak FORTRAN czy BASIC. Współcześnie, w języku Bash, ta funkcjonalność jest wciąż bardzo popularna i szeroko wykorzystywana w różnego rodzaju skryptach i programach.

Alternatywą dla interpolarowania stringów jest konkatenacja, czyli po prostu łączenie różnych ciągów znaków. Jednak w przypadku bardziej złożonych operacji i wyrażeń, interpolacja jest zdecydowanie bardziej wygodna.

Wewnątrz Shella, do parsowania wyrażeń matematycznych używana jest polecenie ```expr```. Podobnie, do interpretowania wyrażeń logicznych wykorzystujemy operator ```$(( ))```. Te informacje mogą okazać się przydatne w przypadku bardziej złożonych skryptów.

## Zobacz również
Chcesz dowiedzieć się więcej o interpolarowaniu stringów w Bash? Polecamy przeczytać dokumentację języka Bash oraz zobaczyć przykłady z GitHuba lub zadań na CodeWars.

To wszystko na dzisiaj. Mam nadzieję, że artykuł ten okazał się dla Was przydatny. Do zobaczenia w kolejnych przygodach z programowaniem!