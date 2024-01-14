---
title:                "Bash: Pisanie testów"
programming_language: "Bash"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/writing-tests.md"
---

{{< edit_this_page >}}

# Dlaczego pisanie testów jest ważne?

Testowanie jest integralną częścią procesu tworzenia oprogramowania i pomaga zapewnić jakość i niezawodność kodu. Dzięki pisaniu testów możemy upewnić się, że nasz kod działa zgodnie z oczekiwaniami. Jest to szczególnie ważne w większych projektach, gdzie zmiany w kodzie mogą mieć nieoczekiwaną lub niepożądaną konsekwencję.

# Jak to zrobić?

Aby napisać testy w Bash, musimy zacząć od zdefiniowania funkcji testowych. Funkcje te powinny zawierać instrukcje warunkowe do porównywania oczekiwanych wyników z faktycznymi wynikami naszego kodu. Możemy również użyć słowa kluczowego `assert` do sprawdzenia poprawności poszczególnych aspektów naszego kodu.

```Bash
# Przykładowa funkcja testowa z użyciem assert
test_function {
    expected_result=2
    actual_result=$(bash_script.sh)
    assert "$expected_result" "$actual_result"
}
```

Możemy również zdefiniować testy jednostkowe, które umożliwią nam testowanie pojedynczych funkcji lub części kodu naszego programu. Aby to zrobić, musimy użyć poleceń takich jak `grep` lub `wc`, aby uzyskać określony wynik z wyjścia naszego programu i porównać go z oczekiwanym wynikiem.

```Bash
# Przykładowa funkcja testowa dla określonej funkcji
unit_test {
    expected_result="Hello World"
    actual_result=$(echo "Hello World")
    if [ "$expected_result" == "$actual_result" ]; then
        echo "Test zakończony sukcesem!"
    else
        echo "Test zakończony niepowodzeniem!"
    fi
}
```

# Głębsze zanurzenie

Podczas pisania testów w Bash, istotnym elementem jest umiejętność tworzenia złożonych i precyzyjnych asercji. Możemy również wykorzystać polecenia takie jak `exit` lub `return` do przerwania kodu w przypadku wystąpienia nieoczekiwanego wyniku. Dodatkowo, warto również zwrócić uwagę na przypisywanie zmiennych i funkcji do zmiennych.

# Zobacz także

- [Bash Testing: Fast, Simple Unit Tests with Bash Test Runner](https://www.compose.com/articles/bash-testing-pro-tip-use-bash-test-runner/)
- [Bash Automated Testing System](https://bats-core.readthedocs.io/en/latest/)
- [Testing in bash. Write your own test framework without any dependency](https://medium.com/@nilnandanand/testing-in-bash-bc24f8dfccc8)