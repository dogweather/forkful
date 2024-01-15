---
title:                "Generowanie losowych liczb"
html_title:           "Bash: Generowanie losowych liczb"
simple_title:         "Generowanie losowych liczb"
programming_language: "Bash"
category:             "Bash"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Dlaczego

Generowanie losowych liczb w Bashu może być przydatne w wielu różnych scenariuszach, od testowania aplikacji po gry i symulacje. Może to również być wygodne wtedy, gdy potrzebujemy szybko wylosować jakieś dane bez konieczności tworzenia specjalnego skryptu.

## Jak to zrobić

### Metoda 1: Korzystając z wbudowanego polecenia `shuf`

Polecenie `shuf` jest częścią standardowych narzędzi Bash i pozwala na losowanie liczb z podanego zakresu. Musimy podać tylko minimalną i maksymalną wartość oraz liczbę liczb do wylosowania. Oto przykład:

```Bash
shuf -i 1-100 -n 5
```

Wygeneruje pięć losowych liczb z zakresu od 1 do 100.

### Metoda 2: Używając wbudowanej zmiennej `$RANDOM`

Bash zawiera wbudowaną zmienną `$RANDOM`, która przechowuje losową liczbę całkowitą z zakresu od 0 do 32767. Możemy wykorzystać tę zmienną do wygenerowania losowych liczb w naszym skrypcie. Oto przykład:

```Bash
echo $((RANDOM%100))
```

Powtarzając to polecenie, będziemy otrzymywać różne losowe liczby z zakresu od 0 do 99.

## Deep Dive

Aby uzyskać większą elastyczność w generowaniu losowych liczb, możemy skorzystać z zewnętrznych bibliotek lub narzędzi, takich jak `awk` lub `sed`. Możemy również wykorzystać wyjście z innych poleceń do generowania liczb, na przykład:

```Bash
echo $(openssl rand -hex 4)
```

Spowoduje to wygenerowanie losowego kodu szesnastkowego o długości 4 znaków.

## Zobacz też

- Dokumentacja polecenia `shuf`: https://linux.die.net/man/1/shuf
- Dokumentacja wbudowanej zmiennej `$RANDOM`: https://linux.die.net/man/1/bash
- Poradnik o generowaniu losowych liczb w Bashu: https://www.shell-tips.com/bash/random-numbers/