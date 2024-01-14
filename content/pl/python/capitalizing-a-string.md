---
title:    "Python: Zmiana wielkości literek w ciągu znaków."
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Pisanie w języku Python może być bardzo przyjemne, ale czasami może również wymagać szlifowania prostych umiejętności, takich jak zmiana wielkości liter w ciągach znaków. W tym wpisie bloga omówimy, dlaczego i jak dokonywać tego działań w sposób efektywny.

## Jak to zrobić

Aby zmienić wielkość liter w ciągu znaków w języku Python, możemy skorzystać z metody `upper()` lub `lower()` w połączeniu z metodą `capitalize()`. Poniższy kod ilustruje obie metody:

```Python
string = "witaj świecie!"
print(string.upper()) # WYNIK: WITAJ ŚWIAT!
print(string.capitalize()) # WYNIK: Witaj świecie!
```

Poza tym, możemy również wykorzystać pętle do zmiany wielkości liter pojedynczych słów w ciągu znaków. Przykładowy kod poniżej:

```Python
string = "to jest testowy ciąg znaków"
new_string = ""

for word in string.split():
  new_string += word.capitalize() + " "

print(new_string) # WYNIK: To Jest Testowy Ciąg Znaków
```

## Głębszy wgląd

W języku Python istnieje wiele metod i funkcji do zmiany wielkości liter w ciągu znaków, takich jak `title()`, `swapcase()`, `casefold()` itp. Każda z nich działa nieco inaczej, więc warto zrobić głębszy research, aby wybrać najbardziej odpowiednią dla naszego konkretnego przypadku.

Ponadto, warto również pamiętać, że zmiana wielkości liter w ciągu znaków może mieć wpływ na wydajność naszego kodu, szczególnie w przypadku dużej ilości danych do przetworzenia.

## Zobacz także

- [Dokumentacja Python: metody do modyfikacji ciągów znaków](https://docs.python.org/3/library/stdtypes.html#string-methods)
- [Tutorial Python: zmiana wielkości liter w ciągu znaków](https://www.digitalocean.com/community/tutorials/how-to-format-strings-in-python-3)

Dziękujemy za przeczytanie naszego wpisu blogowego na temat zmiany wielkości liter w języku Python. Mamy nadzieję, że teraz już wiesz, dlaczego i jak dokonywać tego działania. Happy coding!