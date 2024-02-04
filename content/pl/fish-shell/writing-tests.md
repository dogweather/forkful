---
title:                "Pisanie testów"
date:                  2024-02-03T19:31:00.502856-07:00
model:                 gpt-4-0125-preview
simple_title:         "Pisanie testów"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?

Pisanie testów w Fish Shell polega na tworzeniu skryptów, które automatycznie uruchamiają Twój kod w celu zweryfikowania jego zachowania względem oczekiwanych wyników. Praktyka ta jest kluczowa, ponieważ zapewnia, że skrypty powłoki działają zgodnie z zamiarem, pozwalając na wczesne wykrywanie błędów i ułatwiając utrzymanie.

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
