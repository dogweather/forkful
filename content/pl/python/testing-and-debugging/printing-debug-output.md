---
date: 2024-01-20 17:53:31.370053-07:00
description: "How to: (Jak to zrobi\u0107:) W latach 90., kiedy IDE by\u0142y mniej\
  \ rozbudowane, print-debugging to by\u0142 chleb powszedni. Dzi\u0119ki prostej\
  \ implementacji, jest nadal\u2026"
lastmod: '2024-04-05T22:50:49.260524-06:00'
model: gpt-4-1106-preview
summary: "(Jak to zrobi\u0107:) W latach 90., kiedy IDE by\u0142y mniej rozbudowane,\
  \ print-debugging to by\u0142 chleb powszedni."
title: "Drukowanie komunikat\xF3w debugowania"
weight: 33
---

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
