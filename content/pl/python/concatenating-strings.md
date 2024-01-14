---
title:                "Python: Konkatenacja stringów"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/concatenating-strings.md"
---

{{< edit_this_page >}}

## Dlaczego

W tym poście przedstawimy, dlaczego łączenie ciągów znaków jest ważną częścią programowania w języku Python i jak możesz skorzystać z tej funkcji w swoich projektach.

## Jak to zrobić

Łączenie ciągów znaków (ang. string concatenation) jest procesem łączenia dwóch lub więcej ciągów znaków w jeden ciąg. W języku Python możesz to zrobić za pomocą operatora "+" lub funkcji "join". Przykładowy kod wyglądałby następująco:

```Python
# Przykład 1 - użycie operatora "+"
name = "Jan"
message = "Cześć, " + name + "! Witaj na naszej stronie."
print(message)

# Przykład 2 - użycie funkcji "join"
words = ["Witaj", "świecie,"]
print(" ".join(words))
```

Po uruchomieniu powyższego kodu, otrzymamy następujący wynik:

```
Cześć, Jan! Witaj na naszej stronie.
Witaj świecie,
```

To tylko proste przykłady, ale łączenie ciągów znaków może mieć wiele zastosowań. Możesz wykorzystać je do generowania powiadomień w programach, tworzenia skomplikowanych wiadomości lub po prostu do tworzenia łatwiejszych w czytaniu ciągów znaków.

## Głębsza analiza

W języku Python istnieje wiele różnych metod łączenia ciągów znaków, w tym funkcja "format" oraz dodatkowe operatory, takie jak "\*", "=" czy "%". Warto także pamiętać, że ciągi znaków są niemutowalnymi obiektami, co oznacza, że po utworzeniu, nie możesz zmienić ich zawartości. Dlatego ważne jest, aby zawsze tworzyć nowe ciągi, zamiast modyfikować istniejące.

## Zobacz też

- [Dokumentacja języka Python o łączeniu ciągów znaków](https://docs.python.org/3/library/string.html#concatenation-and-repetition)
- [Poradnik na temat manipulacji ciągami znaków w Pythonie](https://realpython.com/python-strings/)
- [Wideo na temat użycia funkcji "join" w Pythonie](https://www.youtube.com/watch?v=VW-58CGCeo4)