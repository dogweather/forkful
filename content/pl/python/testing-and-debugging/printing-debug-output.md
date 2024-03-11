---
date: 2024-01-20 17:53:31.370053-07:00
description: "Drukowanie wynik\xF3w debugowania to pokazywanie warto\u015Bci zmiennych,\
  \ stanu aplikacji, czy akcji w konsoli. Programi\u015Bci robi\u0105 to, aby szybko\
  \ diagnozowa\u0107\u2026"
lastmod: '2024-03-11T00:14:08.125486-06:00'
model: gpt-4-1106-preview
summary: "Drukowanie wynik\xF3w debugowania to pokazywanie warto\u015Bci zmiennych,\
  \ stanu aplikacji, czy akcji w konsoli. Programi\u015Bci robi\u0105 to, aby szybko\
  \ diagnozowa\u0107\u2026"
title: "Drukowanie komunikat\xF3w debugowania"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)
Drukowanie wyników debugowania to pokazywanie wartości zmiennych, stanu aplikacji, czy akcji w konsoli. Programiści robią to, aby szybko diagnozować problemy i śledzić, co się dzieje w kodzie.

## How to: (Jak to zrobić:)
```python
# Podstawowe wyświetlanie komunikatów
print("Hello, Debug!")

# Wyświetlanie zmiennej
a = 5
print(f"Zawartość zmiennej a: {a}")

# Wyświetlanie wielu wartości
pies = "Burek"
wiek = 4
print(f"Mój pies {pies} ma {wiek} lata.")
```
Output:
```
Hello, Debug!
Zawartość zmiennej a: 5
Mój pies Burek ma 4 lata.
```

## Deep Dive (Dogłębna analiza)
W latach 90., kiedy IDE były mniej rozbudowane, print-debugging to był chleb powszedni. Dzięki prostej implementacji, jest nadal używane, choć istnieją alternatywy jak debuggery czy loggery.

Debuggery pozwalają zatrzymywać wykonanie programu, sprawdzać stan, przechodzić przez kod krok po kroku. W Pythonie popularny jest moduł `pdb`.

Logging zamiast przytłaczać konsolę, zapisuje informacje do plików log. Uzywa się `logging` w Pythonie, co pozwala na ustawienie poziomów ważności komunikatów i elastyczną konfigurację.

```python
import logging

# Konfiguracja loggera
logging.basicConfig(level=logging.INFO)

# Logowanie komunikatu
logging.info("To jest informacja dla ciebie")

# Logowanie błędu
try:
    1 / 0
except ZeroDivisionError as e:
    logging.error("Ups, coś poszło nie tak: %s", e)
```

## See Also (Zobacz także)
- Dokumentacja Python `print`: https://docs.python.org/3/library/functions.html#print
- Moduł `logging`: https://docs.python.org/3/library/logging.html
- Moduł `pdb` - Python Debugger: https://docs.python.org/3/library/pdb.html
- Poradnik do `logging`: https://docs.python.org/3/howto/logging.html
