---
title:                "Fish Shell: Pisanie testów"
programming_language: "Fish Shell"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/writing-tests.md"
---

{{< edit_this_page >}}

## Dlaczego pisać testy w Fish Shell?

Testy są niezbędnym elementem każdego programu, a pisząc je w Fish Shell możemy mieć pewność, że nasza aplikacja będzie działać poprawnie. Dodatkowo, dzięki testom zyskujemy większą pewność i łatwiej nam odnaleźć ewentualne błędy w kodzie.

## Jak pisać testy w Fish Shell

```Fish Shell
# Tworzenie testu przy użyciu funkcji "test-equal"
test-equal "Przykładowy test" "hello world" "hello world"

# Kod, który zostanie uruchomiony jeśli test nie przejdzie
if not status-is-success
  echo "Test nie powiódł się" 
end
```

W powyższym przykładzie użyto funkcji "test-equal" do sprawdzenia czy łańcuch "hello world" jest równy samemu sobie. Jeśli test przejdzie, funkcja "status-is-success" zwróci wartość 0, w przeciwnym razie wyświetli się informacja o niepowodzeniu.

Możemy także wykorzystać instrukcje "set" oraz "test" do tworzenia bardziej skomplikowanych testów:

```Fish Shell
set -l number 5
test $number -eq 5
```

W powyższym przypadku, jeśli wartość zmiennej "number" jest równa 5, funkcja "test" zwróci wartość 0, co oznacza powodzenie testu.

## Deep Dive: Jak pisać testy w Fish Shell

Pisanie testów w Fish Shell jest bardzo proste i intuicyjne. Możemy wykorzystać różne funkcje, takie jak "test-equal" czy "test", aby sprawdzać czy nasz kod działa poprawnie. Warto także stosować instrukcję "status-is-success", która pozwala nam na obsługę błędów w przypadku niepowodzenia testu.

Warto także zwrócić uwagę na fakt, że funkcje testowe są bardzo przydatne podczas pracy w zespole, ponieważ pozwalają nam na szybkie i skuteczne weryfikowanie kodu.

## Zobacz także

- [Dokumentacja Fish Shell - test](https://fishshell.com/docs/current/cmds/test.html)
- [Przewodnik po Fish Shell](https://fishshell.com/docs/current/tutorial.html)
- [Narzędzia do testowania w Fish Shell](https://fishshell.com/docs/current/tutorial.html#testing_tools)