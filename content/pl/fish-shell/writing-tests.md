---
title:                "Pisanie testów"
html_title:           "Fish Shell: Pisanie testów"
simple_title:         "Pisanie testów"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/writing-tests.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Pisanie testów jest procesem weryfikacji poprawności kodu przez programistów. Pozwala to upewnić się, że nasza aplikacja działa zgodnie z oczekiwaniami oraz wykryć ewentualne błędy. Jest to również dobra praktyka, która pomaga w utrzymaniu naszego kodu i ułatwia wprowadzanie zmian w przyszłości.

## Jak to zrobić:

Tworzenie testów w Fish Shell jest bardzo proste dzięki specjalnym funkcjom zawartym w tym języku. Aby pisać testy, musimy użyć polecenia `eval` oraz operatora `=` do porównywania oczekiwanego wyniku z rzeczywistym wynikiem naszego kodu.

Przykładowy kod:

```Fish Shell
function add_numbers
    set -l a $argv[1]
    set -l b $argv[2]
    echo $(( $a + $b ))
end

eval (add_numbers 5 6) = 11
```

W powyższym przykładzie tworzymy funkcję `add_numbers`, która dodaje dwie liczby i zwraca wynik. Następnie, za pomocą polecenia `eval` sprawdzamy, czy wynik dodawania 5 i 6 jest równy 11. Jeśli tak, to test zostanie zaliczony, a jeśli nie – zostanie zgłoszony błąd.

## W głębi obiektu:

Testy są nieodłączną częścią programowania od dawna. Jedną z alternatyw dla pisania testów jest manualne sprawdzanie poprawności działania naszego kodu, jednak jest to bardzo czasochłonne i narażone na błędy. Dlatego też pisanie testów jest uważane za standardową i zalecaną praktykę w dzisiejszym świecie programowania.

W Fish Shell mamy do dyspozycji wiele funkcji i narzędzi do tworzenia testów, takich jak `setup`, `teardown` czy `expect_failure`. Możemy również tworzyć testy jednostkowe za pomocą modułu `fish_run_tests`, który wykonuje wszystkie testy w danym skrypcie.

## Zobacz także:

Więcej informacji na temat pisania testów znajdziesz w oficjalnej dokumentacji Fish Shell: https://fishshell.com/docs/current/cmds/eval.html

Jeśli jesteś początkującym programistą, zalecamy zapoznać się z przydatnymi narzędziami i praktykami w tworzeniu testów w Fish Shell: https://github.com/fish-shell/fish-shell/tree/master/tests/specs

Ważna uwaga: pisząc testy, pamiętaj, że kody źródłowe w Fish Shell są wrażliwe na formatowanie, a niektóre znaki mogą mieć inne znaczenie w zależności od kontekstu. Warto prześledzić możliwe przypadki testowe przed przystąpieniem do pisania testów.