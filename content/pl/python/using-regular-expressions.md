---
title:    "Python: Używanie wyrażeń regularnych"
keywords: ["Python"]
---

{{< edit_this_page >}}

# Dlaczego warto poznać wyrażenia regularne?

Wyrażenia regularne są potężnym narzędziem w programowaniu, które pomaga w wyszukiwaniu, wyciąganiu i manipulowaniu tekstem. Dzięki nim, można znacznie przyspieszyć pracę z dużymi plikami tekstowymi, a także przetwarzać dane w bardziej skomplikowany sposób niż przy użyciu prostych funkcji. Poznaność z wyrażeniami regularnymi jest niezbędna dla każdego programisty, który zajmuje się obróbką tekstową.

## Jak używać wyrażeń regularnych w Pythonie?

Najpierw, należy zaimportować moduł re odpowiedzialny za obsługę wyrażeń regularnych: 

```Python
import re
```

Aby wyszukać dopasowanie dla danego wzorca, używamy funkcji `re.search()`:

```Python
result = re.search(r"pla+y", "python")
```

Następnie, możemy sprawdzić czy dopasowanie zostało znalezione oraz wyświetlić dopasowany fragment tekstu:

```Python
if result:
    print("Znaleziono dopasowanie dla słowa 'python'")
    print("Dopasowany fragment tekstu:", result.group())
```

**Output:**
```
Znaleziono dopasowanie dla słowa 'python'
Dopasowany fragment tekstu: python
```

Wyrażenia regularne posiadają również specjalne znaki, które pozwalają na bardziej zaawansowane wyszukiwanie. Na przykład, `+` oznacza jeden lub więcej wystąpień danego znaku, `?` oznacza zero lub jedno wystąpienie, a `*` oznacza zero lub więcej wystąpień. 

Należy pamiętać również o znaku `r` przed wzorcem, który oznacza, że jest on interpretowany jako znaki tekstowe bez specjalnych znaczeń.

## Wskazówki i ciekawostki dotyczące wyrażeń regularnych

- Aby dokonać dopasowania z uwzględnieniem wielkości liter, należy dodać flagę `re.IGNORECASE` do funkcji `re.search()`.
- Można również użyć wyrażeń regularnych w funkcji `sub()` do zastępowania tekstów, na przykład: `re.sub(r"ko+t", "kot", "kot kotek")` zwróci tekst "kot kotek", ponieważ zastąpi wszystkie wystąpienia słowa "kot" z uwzględnieniem dodatkowego znaku "k".
- Aby sprawdzić, czy dany ciąg znaków jest całkowicie dopasowany do danego wzorca, należy użyć znaków `^` na początku wzorca oraz `$` na końcu, na przykład: `r"^[a-z]$"` będzie dopasowane do pojedynczej małej litery.

# Zobacz również

- [Oficjalna dokumentacja Pythona dotycząca wyrażeń regularnych](https://docs.python.org/3/library/re.html)
- [10 przykładów zastosowań wyrażeń regularnych w Pythonie](https://realpython.com/regex-python/)
- [Szczegółowy kurs wyrażeń regularnych w Pythonie](https://regexone.com/references/python)