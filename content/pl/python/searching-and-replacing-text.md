---
title:                "Wyszukiwanie i zastępowanie tekstu"
html_title:           "Python: Wyszukiwanie i zastępowanie tekstu"
simple_title:         "Wyszukiwanie i zastępowanie tekstu"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Co to jest i dlaczego programiści tego potrzebują?

W przeciągu swojej kariery jako programista, często będziesz musiał modyfikować duże ilości tekstu w swoim kodzie. Nie chcesz ręcznie zmieniać każdego wystąpienia danego słowa lub frazy, ponieważ jest to czasochłonne i podatne na błędy. Dlatego programiści korzystają z techniki wyszukiwania i zamiany tekstu, która automatycznie zastępuje określone słowa lub frazy w całym kodzie.

# Jak to zrobić?

Możesz użyć metody ```replace()``` w Pythonie, aby zamienić wystąpienia wybranego tekstu na inny. Oto przykładowy kod w języku Python:

```
text = "To jest przykładowy tekst do zmiany."
replaced_text = text.replace("przykładowy", "nowy")
print(replaced_text)

# Output: To jest nowy tekst do zmiany.
```

Możesz również wykorzystać wyrażenia regularne, aby precyzyjniej wybrać fragment tekstu do zamiany. Przykładowy kod wykorzystujący moduł ```re``` w Pythonie wyglądałby tak:

```
import re

text = "To jest przykładowy tekst do zmiany, ale tylko niektóre wyrazy."
replaced_text = re.sub(r"przykładowy|wyrazy", "nowy", text)
print(replaced_text)

# Output: To jest nowy tekst do zmiany, ale tylko niektóre nowe.
```

# Pogłębiony przegląd

Wyszukiwanie i zamiana tekstu jest techniką, która jest stosowana od bardzo dawna w programowaniu. Pierwsze komputery używały "kart perforowanych" do wykonywania tych operacji, a obecnie wykorzystujemy specjalne narzędzia w naszych edytorach kodu.

Alternatywą dla wyszukiwania i zamiany może być też "refaktoryzacja kodu", czyli zmiana struktury lub układu kodu w celu poprawy jego czytelności lub wydajności. Jednak w przypadku prostych zmian tekstowych, wyszukiwanie i zamiana jest najczęściej używaną metodą.

W klasycznej wersji Pythona, metoda ```replace()``` jest bezpieczna, ale nie jest w stanie zastąpić ciągów znaków uwzględniając ich wielkość liter. W takiej sytuacji lepiej jest wykorzystać metodę ```replace()``` z modułem ```re```, która pozwala na dopasowanie i zamianę ciągów znaków niezależnie od ich wielkości.

# Zobacz też

Jeśli chcesz dowiedzieć się więcej o używaniu wyrażeń regularnych w Pythonie, możesz zapoznać się z dokumentacją do modułu ```re```. Inne przydatne narzędzia do wyszukiwania i zmiany tekstu w kodzie to, na przykład, Narzędzie Wyszukiwania i Zamiany w edytorze kodu PyCharm.