---
title:    "Fish Shell: Pisanie testów"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/writing-tests.md"
---

{{< edit_this_page >}}

## Dlaczego warto pisać testy w języku Fish Shell?

Pisanie testów jest kluczowym elementem w procesie pisania oprogramowania. Dzięki nim możemy szybko sprawdzić poprawność działania naszego kodu i uniknąć błędów w produkcji. W języku Fish Shell, pisanie testów jest prostsze niż się wydaje, a może przynieść wiele korzyści. W tym artykule dowiecie się, dlaczego warto pisać testy w języku Fish Shell i jak to zrobić.

## Jak pisać testy w języku Fish Shell?

Aby pisać testy w języku Fish Shell, należy użyć wbudowanego w niego frameworka testującego - `test`. Możemy używać go do testowania pojedynczych funkcji lub skryptów, a także całych plików lub projektów. Poniżej znajdują się przykłady wykorzystania `test` w różnych sytuacjach.

### Testowanie pojedynczej funkcji

Załóżmy, że mamy prostą funkcję, która dodaje dwa argumenty i zwraca wynik. Chcemy przetestować, czy zwraca poprawną wartość dla różnych kombinacji argumentów. Możemy to zrobić tak:

```Fish Shell
function add -a n1 -a n2
    return $n1 + $n2
end

test "Test dodawania liczb" -e add 2 3; and test (add 5 10) -eq 15
```

W wyniku powinniśmy uzyskać:

```
> fish test.fish
1 test, żadne nie wywołuje błędów
```

W przypadku, gdy funkcja zwróciłaby niepoprawny wynik, zobaczylibyśmy komunikat o błędzie.

### Testowanie skryptów i plików

`test` możemy również wykorzystać do testowania całości skryptów lub plików. Załóżmy, że mamy plik `hello_world.fish`, który wyświetla na ekranie "Hello world!". Chcemy przetestować, czy tak się dzieje. Możemy to zrobić tak:

```Fish Shell
test "Testowanie skryptu hello_world.fish" -e fish hello_world.fish; and test (fish hello_world.fish) = "Hello world!"
```

W kolejnych linijkach możemy przetestować różne scenariusze i oczekiwać różnych wyników, a następnie uruchomić całe testy jedną komendą - `fish test.fish`.

## Vertica Deep Dive

W języku Fish Shell mamy dostępne wiele funkcji i udogodnień, które mogą ułatwić pisanie testów. Możemy na przykład sprawdzać, czy dane polecenie wywołuje błąd, czy czyta poprawnie pliki lub zmienne i wiele więcej. Warto przejrzeć dokumentację oraz eksperymentować z różnymi możliwościami.

## Zobacz również

- Dokumentacja Fish Shell: https://fishshell.com/docs/current/
- Oficjalny framework testujący Fish Shell: https://github.com/fishworks/test