---
title:    "Python: Łączenie ciągów znaków"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/python/concatenating-strings.md"
---

{{< edit_this_page >}}

## Dlaczego

Ciągłe rozszerzanie możliwości języka Python jest jednym z głównych powodów, dla których warto uczyć się tego języka programowania. Jedną z najważniejszych umiejętności, które są niezbędne do rozwoju w programowaniu, jest umiejętność łączenia ze sobą ciągów znaków, również znane jako konkatenacja. W tym artykule pokażę Ci, dlaczego warto nauczyć się tej umiejętności oraz jak w prosty sposób jej używać w Pythonie.

## Jak to zrobić

Aby dokonać konkatenacji dwóch ciągów znaków w Pythonie, użyj operatora "+" lub metody "join()". Operator "+" umożliwia łączenie tylko dwóch ciągów, natomiast metoda "join()" może być użyta do łączenia wielu ciągów. Przykładowy kod wyglądałby tak:

```Python
string1 = "Witaj"
string2 = "w świecie"
result = string1 + string2
print(result)
```
Output:
```Python
Witaj w świecie
```

Jeśli chcesz użyć metody "join()", musisz określić separator, który będzie rozdzielał łączone ciągi. Przykładowy kod:

```Python
lista = ["Dzień", "dobry", "w", "Pythonie"]
result = " ".join(lista)
print(result)
```

Output:
```Python
Dzień dobry w Pythonie
```

## Głębsze zagadnienia

W Pythonie istnieje również możliwość formatowania łańcuchów znaków przy użyciu metody "format()". Jest to przydatne w przypadku, gdy chcesz wstawiać zmienne do już istniejących ciągów znaków. Przykładowy kod:

```Python
name = "Kasia"
age = 22
message = "Cześć, mam na imię {} i mam {} lata".format(name, age)
print(message)
```

Output:
```Python
Cześć, mam na imię Kasia i mam 22 lata
```

Innym ciekawym aspektem jest możliwość łączenia nie tylko ciągów znaków, ale także innych typów danych, takich jak liczby. Jest to możliwe dzięki funkcji "str()", która konwertuje wartość na ciąg znaków. Przykładowy kod:

```Python
num1 = "5"
num2 = 10
result = int(num1) + num2
print(result)
```

Output:
```Python
15
```

## Zobacz również

- [Python String Concatenation](https://www.w3schools.com/python/gloss_python_string_concatenation.asp)
- [The Ultimate Guide to Python Strings and String Manipulation](https://realpython.com/python-strings/)
- [Working with Strings in Python](https://www.geeksforgeeks.org/working-with-strings-in-python/)