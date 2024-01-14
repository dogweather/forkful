---
title:    "Python: Odczytywanie pliku tekstowego"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek zastanawiałeś się, jak Python może pomóc w czytaniu plików tekstowych? W tym wpisie dowiesz się, dlaczego jest to ważna umiejętność dla każdego programisty, a także jakie techniki można zastosować, aby podnieść swoje umiejętności czytania plików tekstowych w Pythonie.

## Jak to zrobić

Aby czytać plik tekstowy w Pythonie, można użyć funkcji readline() lub readlines(). Pierwsza z nich wczytuje pojedynczą linię tekstu, podczas gdy druga odczytuje cały plik i zwraca listę linii. Przykładowy kod:

```Python
plik = open("tekstowy_plik.txt")
# wczytaj pierwszą linię
linia = plik.readline()
print(linia)     # wyświetli zawartość linii
# odczytaj cały plik i zwróć listę linii
lista_linii = plik.readlines()
print(lista_linii)    # wyświetli listę linii
plik.close()    # zawsze należy zamknąć plik po użyciu
```

## Dogłębna analiza

W Pythonie pliki tekstowe są traktowane jako obiekt plikowy. Oznacza to, że można użyć różnych metod na otwartym pliku, takich jak readline() i readlines(). Ponadto, funkcja with open() jest często preferowaną metodą otwierania pliku, ponieważ zamyka plik automatycznie po zakończeniu działania bloku kodu.

Czytanie plików tekstowych jest bardzo ważną umiejętnością dla każdego programisty. W drodze do doskonałości warto również zapoznać się z innymi funkcjami i metodami związanymi z plikami tekstowymi, takimi jak write() i seek().

## Zobacz również

- [Oficjalna dokumentacja Pythona o czytaniu i zapisywaniu plików tekstowych](https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files)
- [Poradnik o operacjach na plikach tekstowych w Pythonie](https://realpython.com/read-write-files-python/)