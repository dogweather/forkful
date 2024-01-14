---
title:                "Python: Łączenie ciągów znaków"
simple_title:         "Łączenie ciągów znaków"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/concatenating-strings.md"
---

{{< edit_this_page >}}

## Dlaczego

Czemu warto uczyć się łączenia ciągów w Pythonie? Odpowiedź jest prosta - łączenie ciągów jest niezbędną umiejętnością w każdym języku programowania, a w Pythonie jest jeszcze bardziej rozbudowane dzięki zastosowaniu dwóch różnych metod: operatora "+" i metody .join(). Pozwala to na elastyczność w pracy z ciągami i ułatwia pisanie czytelnego i funkcjonalnego kodu.

## Jak to zrobić

Łączenie ciągów w Pythonie jest proste i intuicyjne. Przy użyciu operatora "+" możemy połączyć dwa lub więcej ciągów, np. "Hello" + " " + "World" = "Hello World". Natomiast przy użyciu metody .join() możemy połączyć elementy z listy w jedną linię tekstu, np. " ".join(["Hello", "World"]) = "Hello World". Poniżej znajdują się przykładowe kody wraz z efektem wyjściowym: 
	
```Python
# przy użyciu operatora "+"
print("Cześć " + " " + "świecie!")
# output: Cześć świecie!

# przy użyciu metody .join()
print(" ".join(["Witaj", "świecie!"]))
# output: Witaj świecie!
```

## Głębsze zagadnienia

W Pythonie ciągi są niezmienialne (immutable), co oznacza, że nie mogą być modyfikowane. Dlatego każde łączenie ciągów tworzy nowy obiekt. W przypadku operatora "+" tworzony jest nowy obiekt, natomiast przy użyciu metody .join() wykorzystywany jest tylko jeden obiekt, co czyni ją bardziej wydajną. Innym ważnym aspektem jest użycie odpowiednich separatorów przy łączeniu ciągów, np. "," lub "\n". W przypadku metody .join() separator wpisywany jest w nawiasie, np. "\n".join(["Witaj", "świecie!"]) = "Witaj\nświecie!". 

## Patrz także

Chcesz dowiedzieć się więcej o łączeniu ciągów w Pythonie? Sprawdź poniższe materiały:

- [Dokumentacja Pythona na temat ciągów](https://docs.python.org/3.8/tutorial/introduction.html#strings)
- [Samouczek na Thema Education o łączeniu ciągów w Pythonie](https://thema-education.com/learn-programming/merge-strings-python/)
- [Wideo tutorial na Youtube o łączeniu ciągów w Pythonie](https://www.youtube.com/watch?v=8c74Oxn6fMA)