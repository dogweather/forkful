---
title:                "Zaokrąglanie liczb"
date:                  2024-01-26T03:44:41.439412-07:00
model:                 gpt-4-0125-preview
simple_title:         "Zaokrąglanie liczb"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/rounding-numbers.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Zaokrąglanie liczb polega na dostosowywaniu wartości do najbliższego określonego miejsca — na przykład z 2,56 do 3, jeśli zaokrąglamy do pełnych liczb. Programiści robią to dla uproszczenia lub aby spełnić określone specyfikacje numeryczne, zazwyczaj aby uniknąć niuansów spowodowanych błędami precyzji liczb zmiennoprzecinkowych lub aby wynik był przyjazny dla użytkownika.

## Jak to zrobić:
W Gleam, zaokrąglanie nie znajduje się w standardowej bibliotece według mojego ostatniego sprawdzenia, ale oto jak zwykle zaokrągla się float do najbliższej liczby całkowitej, używając bezpośrednio funkcji Erlanga:

```gleam
external fn erlang_round(Float) -> Int = "erlang" "round"

pub fn main() {
  let rounded = erlang_round(2.56)
  rounded // Wyniki: 3
}
```

Wyjście:
```
3
```

Masz inną precyzję na myśli? Powiedzmy, zaokrąglenie do dwóch miejsc po przecinku? Potrzebujemy trochę matematyki:

```gleam
pub fn round_to_two_places(num: Float) -> Float {
  let multiplier = 100.0
  let tmp = num * multiplier
  let round_tmp = erlang_round(tmp)
  round_tmp / multiplier
}

pub fn main() {
    round_to_two_places(2.569) // Wyniki: 2.57
}
```

Wyjście:
```
2.57
```

## Wgłębiając się
Historycznie, zaokrąglanie liczb było kluczowe, szczególnie w obliczeniach finansowych i naukowych, gdzie precyzja i standardy mają ogromne znaczenie. Bez zaokrąglania dostalibyśmy wszędzie nieprzyjemnie długie dziesiętne, co czyniłoby obliczenia niepraktycznymi i podatnymi na błędy.

W świecie programowania różne języki oferują różne podejścia, od wbudowanych funkcji po kompleksowe biblioteki matematyczne. Zaokrąglanie może obejmować różne zasady – na przykład "zaokrąglanie w górę" (zwykła metoda) lub "zaokrąglanie do najbliższej parzystej" (często używane w obliczeniach finansowych, aby uniknąć stronniczości).

Gleam, będąc młodym językiem z korzeniami w Erlangu, opiera się na solidnym zestawie funkcji numerycznych Erlanga. Wraz z rozwojem języka, możemy zobaczyć wprowadzenie rodzimych funkcji, zmniejszając potrzebę wywoływania zewnętrznych rutyn.

## Zobacz również
- Moduł :math Erlanga dla większego kręcenia liczbami: https://erlang.org/doc/man/math.html
- Aby zrozumieć, dlaczego zaokrąglanie może być zawiłe, Standard Pływających Punktów IEEE: https://ieeexplore.ieee.org/document/8766229
- Zainteresowany matematyką za tym stojącą? Sprawdź "Co każdy informatyk powinien wiedzieć o arytmetyce liczb zmiennoprzecinkowych": https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html
