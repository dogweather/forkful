---
title:                "Pisanie testów"
html_title:           "Bash: Pisanie testów"
simple_title:         "Pisanie testów"
programming_language: "Bash"
category:             "Bash"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/writing-tests.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Testowanie kodu to proces sprawdzania jego poprawności i działania. Programiści robią to, aby upewnić się, że ich kod działa prawidłowo i aby uniknąć błędów w przyszłości.

## Jak to zrobić:
Poniżej znajdują się przykłady kodu i wyniku, które wyjaśnią, jak pisać testy w Bash. 

```Bash 
#!/bin/bash

# Przykładowy skrypt, którego wynik chcemy przetestować
# Ten skrypt po prostu wyświetla "Hello World"

echo "Hello World"
```

```Bash
#!/bin/bash

# Skrypt testowy
# Sprawdzamy, czy wynik wyświetla się poprawnie

EXPECTED="Hello World" 
# zmienna z oczekiwanym wynikiem

OUTPUT=$(./hello_world.sh)
# przypisujemy wynik skryptu do zmiennej

if [ "$OUTPUT" == "$EXPECTED" ]; then 
    # sprawdzamy, czy wyniki są takie same
    echo "Test passed!" 
    # jeśli tak, wyświetlamy komunikat o poprawnym teście
else
    echo "Test failed!" 
    # jeśli nie, wyświetlamy komunikat o nieudanym teście
fi
```

## Deep Dive:
Testowanie kodu jest częścią rozwoju oprogramowania, które powstało na początku lat 50. XX wieku. Alternatywą dla pisania testów w Bash jest użycie narzędzia do automatyzacji testów, np. Selenium. W Bash możemy pisać testy jednostkowe dla pojedynczych funkcji w skrypcie lub testować całe skrypty na różnych systemach operacyjnych.

## Zobacz także:
- [Bash nieformalny podręcznik](http://intertwingly.net/projects/bashcmd/) - przydatna strona z instrukcjami i przykładami dla początkujących użytkowników Bash.
- [SeleniumHQ](https://www.seleniumhq.org/) - narzędzie do automatyzacji testów dla stron internetowych.
- [Testowanie jednostkowe w Bash](https://github.com/lehmannro/assert.sh) - biblioteka pozwalająca na pisanie testów jednostkowych w Bash.