---
title:                "Usuwanie znaków pasujących do wzorca"
html_title:           "Python: Usuwanie znaków pasujących do wzorca"
simple_title:         "Usuwanie znaków pasujących do wzorca"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli pracujesz z dużymi danymi lub tekstami, często możesz być zmuszony do usuwania części tekstu, które pasują do określonego wzoru. Może to być przydatne w celu wyczyszczenia danych lub aby ukryć poufne informacje. W tym artykule dowiesz się, jak usunąć znaki pasujące do wzoru w języku Python.

## Jak To Zrobić

Możesz użyć funkcji `re.sub()` z modułu `re` w celu usunięcia znaków pasujących do wzoru. Poniżej znajduje się przykładowy kod:

```Python
import re

text = "To jest przykładowy tekst, który wymaga usunięcia niektórych znaków!"
clean_text = re.sub("[^a-zA-Z0-9 ]", "", text)

print(clean_text)
```

W powyższym przykładzie używamy wzorca `[a-zA-Z0-9 ]` aby usunąć wszystkie znaki, które nie są literami, cyframi lub spacją. Możesz dostosować ten wzorzec do swoich potrzeb. Następnie tworzymy nowy ciąg znaków przy użyciu funkcji `re.sub()`, przekazując pierwotny tekst oraz wzorzec. W ten sposób usuniemy wszystkie pasujące znaki i otrzymamy czysty tekst.

## Głębszy Wgląd

W języku Python możesz używać wielu różnych wzorców do wykrywania i usuwania znaków pasujących do określonego wzoru. Na przykład, jeśli chcesz usunąć tylko znaki specjalne, możesz użyć wzoru `[!@#$%^&*(),.?":{}|<>]`. Możesz także użyć wyrażeń regularnych w celu bardziej zaawansowanej manipulacji tekstem.

Funkcja `re.sub()` może również przyjmować funkcję jako argument, co pozwala na bardziej zaawansowane przetwarzanie tekstu. Możesz także użyć modułu `re` do zastępowania znaków pasujących do wzoru przez inny ciąg znaków, a nie tylko usuwać je całkowicie.

## Zobacz Również

- [Dokumentacja modułu `re` w języku Python](https://docs.python.org/3/library/re.html)
- [Wyrażenia regularne w Pythonie - wideo tutorial](https://www.youtube.com/watch?v=K8L6KVGG-7o)
- [Tutorial: Jak pracować z wyrażeniami regularnymi w języku Python](https://realpython.com/regex-python/)