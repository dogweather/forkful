---
title:    "Bash: Wydobywanie podciągów"
keywords: ["Bash"]
---

{{< edit_this_page >}}

# Dlaczego warto używać funkcji substr na Bashu?

Często w pracy programisty konieczne jest manipulowanie ciągami tekstowymi, wycinanie z nich określonych fragmentów lub zmiana ich długości. W takich sytuacjach przydaje się funkcja `substr`, która pozwala na wyodrębnienie podciągu znaków w przekazanym tekście. Dzięki temu narzędziu można w szybki i prosty sposób przetwarzać dane oraz usuwać zbędne fragmenty ciągów tekstowych.

## Jak używać funkcji substr w Bashu?

Funkcja substr przeprowadza wycięcie fragmentu tekstu z podanego ciągu według określonych parametrów. Aby jej użyć, należy podać ciąg tekstowy oraz indeks początkowy i opcjonalnie indeks końcowy. Warto zauważyć, że numeracja indeksów w Bashu zaczyna się od 0. Dla przykładu, jeśli chcemy otrzymać podciąg od 3 do 8 znaku w ciągu "Hello World", użyjemy poniższej komendy:

```Bash
echo ${var:3:5}
```

W tym przypadku wyjściem będzie "lo Wo".

## Głębszy wgląd w funkcję substr

Funkcja substr w rzeczywistości wykorzystuje parametry podane do kodu `VAR[OFFSET]:[LENGTH]`. W przypadku, gdy nie podamy długości, wybrany będzie cały fragment od danego indeksu do końca ciągu tekstowego. Istnieje również możliwość użycia wartości ujemnych dla indeksów, co spowoduje liczenie od końca ciągu. Warto również zauważyć, że funkcja substr nie zmienia wartości zmiennej, tylko zwraca przetworzony ciąg jako wynik.

# Zobacz również

- [Dokumentacja funkcji substr w Bashu](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html#Shell-Parameter-Expansion)
- [Przykładowe wykorzystanie funkcji substr](https://www.cyberciti.biz/faq/bash-substring/)
- [Inne przydatne funkcje dla pracy z ciągami tekstowymi w Bashu](https://www.thegeekstuff.com/2010/07/bash-string-manipulation/)