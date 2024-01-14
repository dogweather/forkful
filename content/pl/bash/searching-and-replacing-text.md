---
title:    "Bash: Wyszukiwanie i zamienianie tekstu"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Dlaczego

Często zdarza się, że w trakcie pisania skryptów lub programów w języku Bash, musimy dokonać zmiany w tekście. Przykładowo, może to być zmiana separatora w pliku CSV lub zamiana nazwy zmiennej. W tych sytuacjach bardzo przydatne jest umiejętne korzystanie z funkcji wyszukiwania i zamiany tekstu. Dzięki temu możemy szybko i precyzyjnie wprowadzać niezbędne zmiany w naszym kodzie.

## Jak to zrobić

Korzystanie z funkcji wyszukiwania i zamiany tekstu w języku Bash jest bardzo proste. Wystarczy użyć wbudowanej komendy "sed", która jest odpowiedzialna za przeprowadzanie operacji na tekście. Poniżej przedstawione są przykłady kodu i wyników dla różnych scenariuszy.

1. Zamiana jednego słowa na inne słowo w całym pliku:
```
Bash sed 's/stary_nowy/g' plik.txt
```
Gdzie "stary" to wyrażenie, które chcemy zastąpić, "nowy" to wyrażenie, na które chcemy je zamienić, a "plik.txt" to nazwa pliku, na którym chcemy przeprowadzić operację.

2. Zamiana wszystkich wystąpień w pliku z użyciem podwójnych cudzysłowi:
```
Bash sed "s/[,]/ [tab] /g" plik.csv
```
W powyższym przykładzie dokonujemy zamiany separatorów "," na "[tab]" w pliku CSV.

3. Wykorzystanie wyrażeń regularnych do bardziej złożonych operacji:
```
Bash sed 's/ha(te|owi)j/tekstu/g' plik.txt
```
W powyższym przykładzie dokonujemy zamiany słów "hatej" lub "hatejowi" na słowo "tekstu" w naszym pliku.

## Warto wiedzieć

Funkcja wyszukiwania i zamiany tekstu w Bash jest bardzo elastyczna i pozwala na dokonywanie złożonych operacji przy wykorzystaniu wyrażeń regularnych. Dodatkowo, komenda "sed" umożliwia używanie flag (opcji) do precyzyjnego określenia zakresu czy też typu operacji. Pełna dokumentacja dostępna jest w manualu systemowym, który można wywołać poleceniem "man sed".

## Zobacz także

- Dokumentacja komendy "sed": https://linux.die.net/man/1/sed
- Przydatne wyrażenia regularne: https://www.regular-expressions.info/quickstart.html
- Praktyczny poradnik korzystania z "sed": https://www.linux.com/training-tutorials/going-deeper-into-sed-linux/