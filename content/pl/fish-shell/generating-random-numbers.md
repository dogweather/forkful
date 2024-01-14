---
title:    "Fish Shell: Generowanie losowych liczb"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Dlaczego

Generowanie liczb losowych jest nieodłączną częścią wielu programów i skryptów. Może to być przydatne do testowania lub losowych wyborów. W tym artykule dowiesz się, jak wygenerować liczby losowe w języku skryptowym Fish Shell.

## Jak to zrobić

Aby wygenerować liczbę losową w Fish Shell, użyjemy polecenia `math` wraz z funkcją `rand`.

```
Fish Shell
math rand -0.5 0.5
```

Ta komenda spowoduje wygenerowanie liczby losowej z zakresu od -0.5 do 0.5. Możesz zmienić ten zakres według potrzeb, podając odpowiednie wartości jako argumenty funkcji `rand`.

Możesz również wygenerować więcej niż jedną liczbę losową, dodając opcję `-c` i podając liczbę żądanych wyników.

```
Fish Shell
math -c 5 rand 0 10
```

Powyższa komenda wygeneruje 5 liczb losowych z przedziału od 0 do 10.

## Pogłębiona analiza

W języku Fish Shell do generowania liczb losowych możemy również użyć polecenia `seq`. Polecenie to przyjmuje trzy argumenty: wartość początkową, wartość końcową i krok. Następnie generuje sekwencję liczb pomiędzy wartością początkową a końcową z określonym krokiem.

```
Fish Shell
seq 1 2 10
```

Komenda ta wygeneruje sekwencję liczb od 1 do 10 z krokiem równym 2.

Możesz również skorzystać z polecenia `head` do wyświetlenia określonej liczby wygenerowanych liczb.

```
Fish Shell
seq 1 1 100 | head -n 10
```

Powyższa komenda wygeneruje sekwencję liczb od 1 do 100 i wyświetli tylko pierwsze 10 liczb.

## Zobacz również

- Dokumentacja polecenia `math` w języku Fish Shell: https://fishshell.com/docs/current/cmds/math.html
- Przykłady użycia funkcji `rand` w języku Fish Shell: https://fishshell.com/docs/current/cmds/math.html#math-rand
- Dokumentacja polecenia `seq` w języku Fish Shell: https://fishshell.com/docs/current/cmds/seq.html