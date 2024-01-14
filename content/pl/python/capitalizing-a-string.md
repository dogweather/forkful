---
title:                "Python: Konwertowanie tekstu na wielkie litery"
simple_title:         "Konwertowanie tekstu na wielkie litery"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek zastanawiałeś się, dlaczego programiści często stosują funkcję "capitalize" w swoim kodzie? W tym artykule dowiecie się, dlaczego konwersja tekstu na wielkie litery jest tak ważna w niektórych przypadkach.

## Jak to zrobić?

Aby skorzystać z funkcji "capitalize" w języku Python, wystarczy użyć wbudowanej metody "capitalize()" na odpowiedniej zmiennej. Kiedy to zrobimy, wszystkie pierwsze litery w stringu zostaną przekształcone na wielkie, a wszystkie pozostałe na małe. Oto prosty przykład:

```Python
tekst = "programowanie w pythonie"
print(tekst.capitalize())
```

Będzie to wydrukowane jako "Programowanie w pythonie". Widzimy tutaj, że pierwsza litera została przekształcona na wielką, a wszystkie pozostałe pozostały niezmienione.

## Głębsza analiza

Poza prostym użyciem "capitalize()", ta funkcja może być szczególnie użyteczna, gdy chcemy ustandaryzować wyświetlane wyniki lub porównywać dane. Na przykład, jeśli mamy dwie zmienne, jedną z wartością "yes" a drugą "YES" i chcemy je porównać, bez użycia "capitalize()" zostanie wyświetlone, że są one różne, ponieważ litery różnią się wielkością. Jednak po zastosowaniu "capitalize()" na obu zmiennych, przyrównanie będzie zwracać wartość True.

## Zobacz również

Jeśli inne funkcje wbudowane w języku Python cię interesują, zapoznaj się z artykułami na temat "join()" i "split()". Aby dowiedzieć się więcej o ogólnych zasadach programowania, polecamy przeczytać "The Pragmatic Programmer: From Journeyman to Master" autorstwa Andrew Hunt i Davida Thomasa.