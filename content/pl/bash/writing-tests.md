---
title:                "Pisanie testów"
date:                  2024-02-03T19:29:36.706359-07:00
model:                 gpt-4-0125-preview
simple_title:         "Pisanie testów"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?
Pisanie testów w Bashu obejmuje tworzenie skryptów testowych w celu weryfikacji funkcjonalności twoich skryptów Bash. Programiści przeprowadzają testy, aby upewnić się, że ich skrypty działają zgodnie z oczekiwaniami w różnych warunkach, wyławiając błędy i usterki przed wdrożeniem.

## Jak to zrobić:
Bash nie posiada wbudowanego frameworka do testowania, ale można pisać proste funkcje testowe. Dla bardziej zaawansowanego testowania popularne są narzędzia stron trzecich, takie jak `bats-core`.

### Podstawowy przykład testu w czystym Bashu:
```bash
function test_example_function {
  result=$(your_function 'test_input')
  expected_output="expected_output"
  
  if [[ "$result" == "$expected_output" ]]; then
    echo "Test zakończony sukcesem."
    return 0
  else
    echo "Test nieudany. Oczekiwano '$expected_output', otrzymano '$result'"
    return 1
  fi
}

# Wywołanie funkcji testowej
test_example_function
```
Przykładowe wyjście:
```
Test zakończony sukcesem.
```

### Używanie `bats-core` do testowania:
Najpierw zainstaluj `bats-core`. Zwykle można to zrobić za pomocą menedżera pakietów lub klonując jego repozytorium.

Następnie napisz swoje testy w oddzielnych plikach `.bats`.

```bash
# Plik: example_function.bats

#!/usr/bin/env bats

@test "test przykładowej funkcji" {
  result="$(your_function 'test_input')"
  expected_output="expected_output"
  
  [ "$result" == "$expected_output" ]
}
```
Aby uruchomić swoje testy, wystarczy wykonać plik `.bats`:
```bash
bats example_function.bats
```
Przykładowe wyjście:
```
 ✓ test przykładowej funkcji

1 test, 0 porażek
```

To podejście pozwala na łatwe zintegrowanie testowania z twoim procesem tworzenia oprogramowania, zapewniając niezawodność i stabilność twoich skryptów Bash.
