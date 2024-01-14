---
title:                "Python: Wyszukiwanie i zamienianie tekstu"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli pracujesz z dużymi ilościami tekstu, wiedza o wyszukiwaniu i zastępowaniu jest niezbędna w twoim arsenale programisty Pythona. Używanie funkcji wyszukiwania i zastępowania pozwala na szybką i efektywną zmianę tekstu, bez potrzeby ręcznego edytowania całego pliku.

## Jak to zrobić

W Pythonie, możesz użyć metody `.replace()` aby dokonać wyszukiwania i zastępowania tekstu. Jeśli na przykład chcesz zmienić wszystkie wystąpienia słowa "kot" na słowo "pies" w naszym tekście, możesz to zrobić w następujący sposób:

```Python
text = "Lubię mojego kota, nazywa się Mruczek"
new_text = text.replace("kot", "pies")
print(new_text)
```
**Output:**
```
Lubię mojego psa, nazywa się Mruczek
```
Wywołanie metody `replace()` na zmiennej `text` spowoduje zamianę wszystkich wystąpień słowa "kot" na słowo "pies". Warto również zauważyć, że metoda ta zwraca nowy ciąg znaków, więc musimy przypisać tę nową wartość do zmiennej `new_text`.

## Wszystkie aspekty poszukiwania i zastępowania tekstu

Podstawowa składnia metody `.replace()` jest następująca:

```Python
string.replace(old, new, count)
```

- `string` to ciąg znaków, na którym chcemy wykonać wyszukiwanie i zastępowanie.
- `old` to ciąg znaków, które chcemy znaleźć i zastąpić. Może to być pojedynczy znak, wyrażenie regularne lub inny ciąg znaków.
- `new` to ciąg znaków, które chcemy wstawić w miejsce `old`.
- `count` (opcjonalne) to liczba wystąpień `old`, które chcemy zamienić. Jeśli nie zostanie podane, wszystkie wystąpienia zostaną zastąpione.

Metoda ta jest również na tyle elastyczna, że pozwala na podmianę nie tylko pojedynczych znaków, ale także całych wyrażeń lub fragmentów tekstu. Aby dowiedzieć się więcej na temat wykorzystania wyrażeń regularnych w metodzie `.replace()`, zobacz ten artykuł [link do artykułu o wyrażeniach regularnych w Pythonie].

## Zobacz również

- [Dokumentacja Pythona - metoda replace()](https://docs.python.org/3/library/stdtypes.html#str.replace)
- [Tutorial o wyrażeniach regularnych w Pythonie](https://www.programiz.com/python-programming/regex)