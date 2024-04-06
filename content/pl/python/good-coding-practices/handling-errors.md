---
date: 2024-01-26 00:57:09.081207-07:00
description: "Jak to zrobi\u0107: Przyk\u0142adowe wyj\u015Bcie po wprowadzeniu nieprawid\u0142\
  owej liczby dla pierwszego bloku."
lastmod: '2024-04-05T21:53:36.404815-06:00'
model: gpt-4-1106-preview
summary: "Przyk\u0142adowe wyj\u015Bcie po wprowadzeniu nieprawid\u0142owej liczby\
  \ dla pierwszego bloku."
title: "Obs\u0142uga b\u0142\u0119d\xF3w"
weight: 16
---

## Jak to zrobić:
``` Python
# Podstawowy blok try-except
try:
    # ryzykowny kod
    liczba = int(input("Wprowadź liczbę: "))
except ValueError:
    # obsługa błędu
    print("To nie jest liczba!")

# Określanie wielu wyjątków
try:
    # kod, który może spowodować różne wyjątki
    wynik = 10 / int(input("Wprowadź dzielnik: "))
except ZeroDivisionError:
    print("Ups! Nie można dzielić przez zero.")
except ValueError:
    print("Potrzebuję liczby, kolego.")

# Użycie else i finally
try:
    liczba = int(input("Wprowadź liczbę do kwadratu: "))
except ValueError:
    print("Mówiłem liczbę!")
else:
    # nie wystąpiły błędy
    print("Twoja liczba podniesiona do kwadratu to:", liczba**2)
finally:
    # zawsze się wykonuje
    print("Dzięki za wypróbowanie!")
```

Przykładowe wyjście po wprowadzeniu nieprawidłowej liczby dla pierwszego bloku:
```
Wprowadź liczbę: cześć
To nie jest liczba!
```

## Szczegółowa analiza
Od zarania programowania obsługa błędów była kluczowa. Wczesne podejścia były prymitywne, na przykład sprawdzanie warunków przed każdą ryzykowną operacją. Składnia `try-except` Pythona wywodzi się z dziedzictwa obsługi wyjątków w starszych językach takich jak C++ i Java, upraszczając proces.

Gdy użyjesz `try` na bloku kodu, Python wypatruje wyjątków. Jeśli pojawi się błąd, blok `except` go przechwytuje. Można być specyficznym co do przechwytywanych wyjątków lub złapać je wszystkie za pomocą gołego `except`. Jednak precyzyjne określanie wyjątków jest lepszą metodą – jest dokładne, nie stanowi siatki dla wszystkiego.

`else` i `finally` są dodatkami do tego koncepcji. Blok `else` wykonuje się, jeśli w bloku try nie wystąpiły błędy. `finally` to niezawodny kolega, który wykonuje się niezależnie od wszystkiego – myśl o operacjach czyszczących.

Alternatywy? Oczywiście, że są. Niektóre języki używają kodów powrotu zamiast wyjątków. Możesz także napotkać instrukcje `with` do obsługi zasobów lub `assertions`, które sprawdzają warunki podczas programowania. Ale kiedy mówimy o solidnych strategiach obsługi błędów, model try-catch wyróżnia się czytelnością i strukturą.

## Zobacz również
Oto kilka dobrych dodatkowych zasobów do jeszcze głębszego zanurzenia się:

- Oficjalna dokumentacja Pythona na temat błędów i wyjątków: [Python Docs – Błędy i wyjątki](https://docs.python.org/3/tutorial/errors.html)
- Przewodnik Real Python na ten temat: [Real Python - Blok try/except/else/finally](https://realpython.com/python-exceptions/)
- Przemyślana dyskusja na temat najlepszych praktyk w obsłudze błędów: [Stack Overflow – Jak prawidłowo ignorować wyjątki?](https://stackoverflow.com/questions/4990718/about-catching-any-exception)
