---
title:                "Python: Konwertowanie daty na ciąg znaków"
programming_language: "Python"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Konwertowanie daty do ciągu znaków jest częstym zadaniem w programowaniu, szczególnie w przypadku tworzenia aplikacji internetowych i baz danych. Może to być przydatne do wyświetlania dat w łatwiejszy do odczytania przez ludzi sposób, lub do przechowywania dat w bazie danych w odpowiednim formacie. W tym artykule dowiesz się jak skutecznie konwertować datę do ciągu znaków przy użyciu języka Python.

## Jak to zrobić

Aby skonwertować datę do ciągu znaków w języku Python, używamy metody `strftime()`. Ta metoda pochodzi z modułu `datetime` i pozwala nam na sformatowanie daty według wybranego wzorca.

```Python
from datetime import datetime

# Utworzenie obiektu datetime z wybranej daty
dt = datetime(2021, 7, 21)

# Konwersja daty do ciągu znaków w formacie "Dzień miesiąca, Rok"
print(dt.strftime("%d/%m/%Y")) # Output: 21/07/2021

# Konwersja daty do ciągu znaków w formacie "Miesiąc Dzień, Rok"
print(dt.strftime("%B %d, %Y")) # Output: July 21, 2021
```

Jak widać z powyższego przykładu, wystarczy użyć metody `strftime()` i określić wybrany przez nas format, aby skutecznie skonwertować datę do ciągu znaków.

## Deep Dive

Podczas konwersji daty do ciągu znaków, ważne jest, aby pamiętać o tym, że niektóre formaty mogą generować nieprawidłowe daty. Na przykład, jeśli użyjemy formatu `%d/%m/%Y` dla daty 9 lutego 2021, otrzymamy ciąg znaków "09/02/2021". Jednak jeśli użyjemy tego samego formatu dla daty 21 lutego 2021, otrzymamy ciąg znaków "21/02/2021". To może być mylące, ponieważ w niektórych krajach standardem jest format "DD/MM/YYYY", a w innych "MM/DD/YYYY". Aby uniknąć takich błędów, zawsze należy uważać na wybrane formaty i mieć na uwadze preferencje regionalne.

Innym ważnym aspektem jest szczegółowa znajomość dostępnych formatów w języku Python oraz dokumentacji dla modułu `datetime`. W tym celu można zapoznać się z listą dostępnych formatów na stronie dokumentacji Python: https://docs.python.org/3/library/datetime.html#strftime-and-strptime-format-codes

## Zobacz też

- Dokumentacja Python dla modułu `datetime`: https://docs.python.org/3/library/datetime.html
- Przewodnik po konwersji daty do ciągu znaków w języku Python: https://www.programiz.com/python-programming/datetime/strftime

Dzięki temu artykułowi powinieneś już wiedzieć jak skutecznie konwertować datę do ciągu znaków za pomocą języka Python. Pamiętaj, aby uważać na wybrane formaty i dostosować je do swoich potrzeb.