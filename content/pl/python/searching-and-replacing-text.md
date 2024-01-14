---
title:                "Python: Wyszukiwanie i zmienianie tekstu"
simple_title:         "Wyszukiwanie i zmienianie tekstu"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Dlaczego

Często podczas pisania kodu zdarza się nam popełnić błędy lub skorzystać z innej nazwy dla zmiennej czy funkcji. W takich sytuacjach bardzo przydatne jest narzędzie do wyszukiwania i zamiany tekstu. Pozwala ono szybko i łatwo odnaleźć wszystkie wystąpienia danego fragmentu tekstu i zamienić je na żądany.

## Jak to zrobić

Do wyszukiwania i zamiany tekstu w Pythonie możemy użyć wbudowanej metody `replace()`. Musimy podać dwa argumenty: tekst, który chcemy zamienić, oraz tekst, na który chcemy go zamienić. W poniższym przykładzie zamienimy wszystkie wystąpienia słowa "pies" na słowo "kot".

```Python
text = "Mam dwa psy, pieska i suczkę."
new_text = text.replace("pies", "kot")
print(new_text)
```

Output:
```
Mam dwa koty, kotka i suczkę.
```

Jeśli chcemy zamienić tylko część wystąpień, możemy podać dodatkowy argument `count`, określający ile razy zamiana ma zostać wykonana. W poniższym przykładzie zamienimy tylko pierwsze dwa wystąpienia słowa "pies" na słowo "kot".

```Python
text = "Mam dwa psy, pieska i suczkę."
new_text = text.replace("pies", "kot", 2)
print(new_text)
```

Output:
```
Mam dwa koty, kotka i suczkę.
```

## Głębszy zanurzenie

Metoda `replace()` działa tylko na pojedynczych zmiennych typu string. Jeśli chcemy wykonać zamianę w większej ilości plików, możemy skorzystać z pętli `for` i funkcji `glob` do odnalezienia odpowiednich plików.

```Python
import glob

for file in glob.glob("*.txt"):
    with open(file, "r") as f:
        text = f.read()
        new_text = text.replace("pies", "kot")
    with open(file, "w") as f:
        f.write(new_text)
```

W powyższym przykładzie wszystkie pliki z rozszerzeniem ".txt" w bieżącym katalogu zostaną otwarte, a następnie wszystkie wystąpienia słowa "pies" zostaną zamienione na słowo "kot". Oryginalne pliki zostaną nadpisane nowym tekstem.

## Zobacz także

- [Oficjalna dokumentacja Pythona](https://docs.python.org/3.9/library/stdtypes.html#str.replace)
- [Przewodnik po pakiecie glob](https://realpython.com/working-with-files-in-python/#the-glob-module)
- [Przydatne metody dla typu string w Pythonie](https://www.programiz.com/python-programming/methods/string)