---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:00.502856-07:00
description: "Pisanie test\xF3w w Fish Shell polega na tworzeniu skrypt\xF3w, kt\xF3\
  re automatycznie uruchamiaj\u0105 Tw\xF3j kod w celu zweryfikowania jego zachowania\
  \ wzgl\u0119dem\u2026"
lastmod: '2024-03-13T22:44:35.844959-06:00'
model: gpt-4-0125-preview
summary: "Pisanie test\xF3w w Fish Shell polega na tworzeniu skrypt\xF3w, kt\xF3re\
  \ automatycznie uruchamiaj\u0105 Tw\xF3j kod w celu zweryfikowania jego zachowania\
  \ wzgl\u0119dem oczekiwanych wynik\xF3w."
title: "Pisanie test\xF3w"
weight: 36
---

## Jak to zrobić:
Fish nie posiada wbudowanego frameworku testowego, jak niektóre inne środowiska programistyczne. Niemniej jednak, możesz pisać proste skrypty testowe, które używają asercji do sprawdzania zachowania Twoich funkcji. Dodatkowo, możesz wykorzystać narzędzia stron trzecich, takie jak `fishtape`, dla bardziej kompleksowego zestawu testowego.

### Przykład 1: Podstawowy skrypt testowy
Zacznijmy od podstawowej funkcji w Fish, która oblicza sumę dwóch liczb:

```fish
function add --description 'Dodaj dwie liczby'
    set -l sum (math $argv[1] + $argv[2])
    echo $sum
end
```

Możesz napisać podstawowy skrypt testowy dla tej funkcji w taki sposób:

```fish
function test_add
    set -l result (add 3 4)
    if test $result -eq 7
        echo "test_add przeszedł"
    else
        echo "test_add nie przeszedł"
    end
end

test_add
```

Uruchomienie tego skryptu powinno dać wynik:

```
test_add przeszedł
```

### Przykład 2: Użycie Fishtape
Dla bardziej solidnego rozwiązania testowego, możesz użyć `fishtape`, generującego TAP runnera testów dla Fish.

Najpierw zainstaluj `fishtape`, jeśli jeszcze tego nie zrobiłeś:

```fish
fisher install jorgebucaran/fishtape
```

Następnie, utwórz plik testowy dla swojej funkcji `add`, np. `add_test.fish`:

```fish
test "Dodawanie 3 i 4 daje 7"
    set result (add 3 4)
    echo "$result" | fishtape
end
```

Aby uruchomić test, użyj następującego polecenia:

```fish
fishtape add_test.fish
```

Przykładowy wynik może wyglądać tak:

```
TAP version 13
# Dodawanie 3 i 4 daje 7
ok 1 - test_add przeszedł
```

To mówi Ci, że test zakończył się sukcesem. `fishtape` pozwala na strukturyzowanie bardziej szczegółowych testów i zapewnia informatywne wyniki, ułatwiając debugowanie i zapewniając kompleksowe pokrycie testowe dla Twoich skryptów Fish.
