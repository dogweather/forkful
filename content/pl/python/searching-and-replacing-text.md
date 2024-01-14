---
title:    "Python: Wyszukiwanie i zamiana tekstu"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/python/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Dlaczego

Kiedy piszemy kod w języku Python, zawsze warto pomyśleć o sposobach, które ułatwią nam pracę i pozwolą zaoszczędzić czas. Jednym z narzędzi, które mogą okazać się bardzo przydatne, jest wyszukiwanie i zastępowanie tekstu. Dzięki temu możemy szybko dokonać zmian w naszym kodzie, bez konieczności ręcznego edytowania wszystkich wystąpień danego fragmentu. W tym artykule opowiemy o tym, jak łatwo wykonać to zadanie w Pythonie.

## Jak to zrobić

Aby wykonać wyszukiwanie i zastępowanie tekstu w Pythonie, możemy skorzystać z kilku różnych metod. Pierwsza z nich to użycie metody `replace()`, która pozwala nam na prostą zamianę jednego słowa na inne. Przykładowo, jeśli chcemy zamienić wszystkie wystąpienia słowa "hello" na "cześć", wpisujemy:

```Python
text = "hello world, hello everyone"
new_text = text.replace("hello", "cześć")
print(new_text)

>> cześć world, cześć everyone
```

Możemy także użyć biblioteki `re`, która udostępnia nam narzędzia do pracy z wyrażeniami regularnymi. Dzięki temu możemy dokonać bardziej złożonych zmian w tekście. Na przykład, jeśli chcemy usunąć wszystkie samogłoski z danego tekstu, możemy użyć następującego kodu:

```Python
import re
text = "Hello World"
new_text = re.sub('[aeiou]', '', text)
print(new_text)

>> Hll Wrld
```

W ten sposób możemy wykonać bardzo różnorodne operacje na tekście, np. zmieniać wielkość liter, wyciągać określone fragmenty tekstu czy usuwać niepotrzebne znaki.

## Wnikliwsze spojrzenie

Jeśli chcemy poznać więcej funkcji związanych z wyszukiwaniem i zastępowaniem tekstu w Pythonie, warto zapoznać się z oficjalną dokumentacją. Tam znajdziemy wiele przykładów oraz opis wszystkich dostępnych metod. Warto także eksperymentować z różnymi wyrażeniami regularnymi, aby dostosować je do swoich potrzeb.

## Zobacz także

- [Dokumentacja Pythona](https://docs.python.org/3/library/stdtypes.html#str.replace)
- [Poradnik na temat wyrażeń regularnych w Pythonie](https://realpython.com/regex-python/)
- [Składnia Markdown](https://www.markdownguide.org/basic-syntax/)