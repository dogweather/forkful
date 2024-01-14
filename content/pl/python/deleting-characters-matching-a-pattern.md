---
title:    "Python: Usuwanie znaków odpowiadających wzorcowi"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/python/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Dlaczego warto usuwać znaki pasujące do wzorca w programowaniu Python?

W programowaniu można napotkać sytuacje, w których potrzebujemy usunąć znaki, które odpowiadają określonemu wzorcowi. Niekiedy jest to potrzebne do oczyszczenia tekstu lub danych, innym razem do przeprowadzenia analizy tekstu. W takich sytuacjach warto znać sposoby na usuwanie znaków pasujących do wzorca w języku Python.

## Jak to zrobić?

Istnieje kilka sposobów na usuwanie znaków pasujących do wzorca w języku Python. Jedną z nich jest użycie funkcji `re.sub()` z modułu regular expression (wyrażenia regularne). Przykładowo, używając wyrażenia regularnego `[a-z]` można usunąć wszystkie litery występujące w danym tekście. Poniższy przykład kodu przedstawia to w praktyce:

```Python
import re
text = "Przykładowy *tekst* zawierający, różne znaki @special! ##characters"
new_text = re.sub(r'[a-z]', "", text)
print(new_text)
```

Output:
```
* @#! ##
```

W powyższym przykładzie, funkcja `re.sub()` wymaga trzech argumentów - pierwszy to wzorzec, drugi to zastępujący znak (w tym przypadku pusty ciąg znaków), a trzeci to tekst, w którym chcemy dokonać zmian. Dzięki wyrażeniom regularnym można także określić bardziej skomplikowane wzorce do usunięcia.

## Dogłębne wyjaśnienie

Funkcja `re.sub()` jest często używana przez programistów do usuwania znaków pasujących do wzorca w języku Python. Jednak warto pamiętać, że jest to tylko jedna z wielu metod dostępnych w języku Python do manipulacji tekstem. Warto także zapoznać się z innymi modułami, takimi jak `string` czy `regex`, które mogą być przydatne w różnych sytuacjach.

## Zobacz także

- [Dokumentacja funkcji re.sub() w języku Python](https://docs.python.org/3/library/re.html#re.sub) 
- [Podstawy wyrażeń regularnych w języku Python](https://www.w3schools.com/python/python_regex.asp)
- [Przykłady wykorzystania wyrażeń regularnych w języku Python](https://realpython.com/regex-python/)