---
title:                "Zaokrąglanie liczb"
aliases:
- pl/python/rounding-numbers.md
date:                  2024-01-26T03:46:23.296325-07:00
model:                 gpt-4-0125-preview
simple_title:         "Zaokrąglanie liczb"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/rounding-numbers.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Zaokrąglanie liczb polega na dostosowywaniu ich do bliższej, prostszej lub bardziej znaczącej wartości. Programiści zaokrąglają liczby, aby upraszczać wyniki, ograniczać miejsca dziesiętne do wyświetlania lub dla pewnych celów matematycznych.

## Jak to zrobić:
Oto podstawowe informacje o zaokrąglaniu liczb w Pythonie:

```python
# Zaokrąglenie liczby do najbliższej liczby całkowitej
print(round(8.67))  # Wyświetla: 9

# Zaokrąglenie liczby do określonej liczby miejsc dziesiętnych
print(round(8.67, 1))  # Wyświetla: 8.7

# Parzyste liczby są zaokrąglane w dół, a nieparzyste liczby są zaokrąglane w górę, gdy są równo oddalone
print(round(2.5))  # Wyświetla: 2
print(round(3.5))  # Wyświetla: 4
```

## Pogłębiona analiza
W Pythonie `round()` nie polega tylko na obcinaniu miejsc dziesiętnych. Historycznie, Python, podobnie jak wiele innych języków, stosuje "zaokrąglenie do najbliższej parzystej" lub "zaokrąglenie bankierskie". Minimalizuje to kumulatywny błąd w sumach lub średnich, co ma znaczenie w obliczeniach finansowych.

Jako alternatywy, dostępne są `math.floor()` i `math.ceil()` z modułu matematycznego Pythona, które obniżają lub podwyższają liczby do najbliższej całkowitej. Ale jeśli szukasz precyzji, `quantize()` z modułu `decimal` pozwala określić zachowanie przy zaokrąglaniu.

W tle, `round()` radzi sobie z liczbami zmiennoprzecinkowymi binarnymi. Ponieważ niektóre dziesiętne nie mogą być dokładnie wyrażone w binarnie, możesz się zdziwić, gdy takie coś jak `round(2.675, 2)` nie stanie się `2.68`, jak oczekiwano. Tu na ratunek przychodzą `decimal` lub `fractions` dla wysokiej precyzji.

## Zobacz także
- Dokumentacja Pythona na temat funkcji wbudowanych: https://docs.python.org/3/library/functions.html#round
- Arytmetyka punktów stałych i zmiennoprzecinkowa w module `decimal`: https://docs.python.org/3/library/decimal.html
- Moduł matematyczny Pythona: https://docs.python.org/3/library/math.html
