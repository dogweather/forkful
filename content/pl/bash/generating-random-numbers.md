---
title:    "Bash: Generowanie losowych liczb"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Dlaczego

Użycie losowych liczb jest niezwykle przydatne w programowaniu, ponieważ pozwala na tworzenie różnego rodzaju symulacji i testów. Mogą również służyć do losowego wybierania elementów w grach czy aplikacjach.

## Jak to zrobić

Aby wygenerować losową liczbę w Bashu, możesz skorzystać z polecenia `echo $RANDOM`. Polecenie to zwraca losową liczbę całkowitą z zakresu od 0 do 32767. Możesz również wykorzystać polecenie `shuf`, które pozwala na generowanie losowych liczb z określonego zakresu, na przykład `shuf -i 1-10 -n 1` wygeneruje losową liczbę z zakresu od 1 do 10.

Możesz również wykorzystać zmienną `$RANDOM` do generowania losowych znaków z ASCII. Przykładowo, `echo $(( $RANDOM % 26 + 97 ))` wygeneruje losową literę z alfabetu.

```Bash
# losowa liczba całkowita z zakresu od 0 do 32767
echo $RANDOM 
# losowa liczba z zakresu od 1 do 10
shuf -i 1-10 -n 1 
# losowa litera z alfabetu
echo $(( $RANDOM % 26 + 97 ))
```

## Głębszy Wgląd

Technika generowania losowych liczb w Bashu opiera się na wykorzystaniu generatora liczb pseudolosowych, który korzysta z tzw. ziarna (ang. seed). Ziarno to zwykle jest liczbą, która definiuje punkt startowy dla generatora, co pozwala na uzyskanie różnych sekwencji liczb.

Aby wykorzystać własne ziarno do generowania losowych liczb w Bashu, możesz użyć opcji `-r` w poleceniu `shuf` oraz opcji `-s` w poleceniu `echo`. Na przykład, `shuf -i 1-10 -n 1 -r 123` wygeneruje losową liczbę z zakresu od 1 do 10, wykorzystując ziarno 123.

```Bash
# losowa liczba z zakresu od 1 do 10, wykorzystując ziarno 123
shuf -i 1-10 -n 1 -r 123
```

Ponadto, istnieje również możliwość skryptowania w Bashu własnego generatora liczb pseudolosowych, co pozwala na większą kontrolę nad wygenerowanymi liczbami. Jedną z popularnych metod jest wykorzystanie algorytmu LCG (ang. Linear Congruential Generator) w połączeniu z pseudo-randomowym ziarnem generowanym przez system.

## Zobacz również

Jeśli jesteś zainteresowany dalszą nauką programowania w Bashu, zachęcamy do zapoznania się z naszymi artykułami na poniższe tematy:

- [Pętle w Bashu](https://linkdoarta1u)
- [Zmienne w Bashu](https://linkdoarta2u)
- [Tworzenie skryptów w Bashu](https://linkdoarta3u)