---
title:    "Bash: Znajdowanie długości ciągu znaków"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Dlaczego

Jeśli jesteś nowym użytkownikiem Bash lub po prostu szukasz sposobu na uzyskanie informacji o długości łańcucha tekstowego, ten artykuł jest dla Ciebie! Poznamy razem kilka sposobów na znalezienie długości łańcucha w Bash i dowiemy się dlaczego jest to ważna umiejętność.

## Jak to zrobić

Aby uzyskać długość łańcucha w Bash, możesz wykorzystać funkcję `expr length` lub `wc -c`. Oto przykładowe kody:

```Bash
# Wykorzystanie funkcji 'expr length'
echo "Hello World" | expr length

# Wykorzystanie polecenia 'wc'
echo "Hello World" | wc -c
```

W pierwszym przypadku, funkcja `expr length` zostanie użyta do obliczenia długości łańcucha "Hello World" (wynik: 11). Natomiast w drugim przypadku, polecenie `wc -c` zwróci liczbę znaków w łańcuchu (wynik: 11).

## Głębszy zanurzenie

Sposób działania funkcji `expr length` polega na wykorzystaniu wyrażenia regularnego, które jest przetwarzane przez interpreter Bash. Natomiast polecenie `wc -c` zlicza wszystkie znaki w łańcuchu, włączając w to spacje i znaki specjalne.

Istnieje także możliwość obliczenia długości łańcucha bez wykorzystania dodatkowych funkcji czy poleceń, poprzez użycie manipulacji zmiennymi i pętli `for`. Możesz też skorzystać z wbudowanej funkcji `${#string}`, która zwróci długość łańcucha bez konieczności wykorzystania żadnych dodatkowych poleceń.

## Zobacz także

- [Dokumentacja Bash](https://www.gnu.org/software/bash/manual/bash.html)
- [Tutorial Bash](https://www.tutorialspoint.com/unix_commands/bash.htm)
- [Kurs Bash na Codecademy](https://www.codecademy.com/learn/learn-bash)