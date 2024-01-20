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

## What & Why?
Testowanie kodu to proces sprawdzania, czy nasze skrypty działają poprawnie. Programiści testują, aby zapobiec błędom, oszczędzać czas i upewnić się, że nowe zmiany nie psują istniejących funkcji.

## How to:
Testowanie w Bashu często odbywa się przez porównanie oczekiwanego wyniku z tym, który zwraca skrypt. Użyj `test` albo nawiasów kwadratowych `[]`.

```Bash
#!/bin/bash
# test_example.sh

expected_output="Hello, World!"
actual_output=$(echo "Hello, World!")

# Testowanie porównaniem stringów
if [ "$expected_output" == "$actual_output" ]; then
    echo "Test passed!"
else
    echo "Test failed!"
fi
```

Wykonanie skryptu wyświetli `Test passed!` jeśli test się powiedzie.

## Deep Dive
W Bashu nie ma wbudowanego zaawansowanego frameworka testowego, ale istnieją narzędzia jak `shunit2` czy `bats` ułatwiające proces. Historia testowania w Bashu sięga jego początków, gdyż nawet proste skrypty potrzebują weryfikacji. Najprostsze formy to porównania wyników lub statusów wyjścia.

## See Also
- Bash Automated Testing System (BATS): https://github.com/bats-core/bats-core
- shUnit2 - xUnit based unit testing for shell scripts: https://github.com/kward/shunit2
- Bash guide for beginners: https://tldp.org/LDP/Bash-Beginners-Guide/html/