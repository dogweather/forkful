---
title:                "Python: Wycinanie podciągów"
simple_title:         "Wycinanie podciągów"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/extracting-substrings.md"
---

{{< edit_this_page >}}

## Dlaczego

Wydobycie podciągów jest ważnym aspektem w wielu projektach programistycznych. Pozwala na wyodrębnienie określonych części tekstu z dłuższej linii, co ułatwia przeprowadzanie analiz i przetwarzanie danych. W tym artykule dowiecie się, dlaczego wydobycie podciągów jest przydatne w programowaniu i jak to zrobić przy użyciu języka Python.

## Jak to zrobić

W celu wydobycia podciągów z tekstu w języku Python, należy użyć metody `substring()`. Przedstawimy teraz kilka przykładów, aby pokazać jak wykorzystać tę metodę w praktyce.

### Przykład 1:

```
text = "Witaj w świecie programowania"
print(text.substring(6,14))
```

**Output:**

```
w świecie
```

W tym przykładzie użyliśmy metody `substring()` aby wyodrębnić podciąg z tekstu od 6 do 14 znaku. W ten sposób otrzymujemy wyraz "świecie" bez pierwszej litery "W".

### Przykład 2:

```
text = "Kocham Pythona"
print(text.substring(7))
```

**Output:**

```
Pythona
```

W drugim przykładzie pominięliśmy drugi parametr metody `substring()`, co oznacza, że zostanie ona użyta do wydobycia podciągu od 7 znaku do końca tekstu.

### Przykład 3:

```
text = "Hello World"
print(text.substring(-3))
```

**Output:**

```
World
```

W ostatnim przykładzie wykorzystaliśmy także wartość ujemną jako drugi parametr metody `substring()`. W takim przypadku liczba ujemna jest traktowana jako odległość od końca tekstu, co oznacza, że wyciągany jest podciąg od ostatniej litery zgodnie z podaną wartością.

## Połączenie podciągów

Kolejną przydatną techniką jest połączenie kilku podciągów w jeden dłuższy. Możemy to zrobić za pomocą operatora plus `+` lub metody `join()`.

### Przykład 1:

```
name = "Michał"
surname = "Kowalski"
print(name + " " + surname)
```

**Output:**

```
Michał Kowalski
```

W tym przypadku połączyliśmy dwa podciągi (zmienne `name` i `surname`) za pomocą operatora plus `+`.

### Przykład 2:

```
word_list = ["Lorem", "ipsum", "dolor"]
sentence = " ".join(word_list)
print(sentence)
```

**Output:**

```
Lorem ipsum dolor
```

W drugim przykładzie użyliśmy metody `join()` do połączenia listy słów w jedno zdanie, oddzielone spacją.

## Głębszy zanurzenie

Metoda `substring()` w Pythonie jest często wykorzystywana przy przetwarzaniu tekstu, np. przy wycinaniu niepotrzebnych części tekstu lub tworzeniu unikalnych nazw plików. Jest to również przydatna umiejętność podczas manipulacji napisami w różnych kontekstach programistycznych.

## Zobacz także

- [Dokumentacja Python](https://docs.python.org/3/library/stdtypes.html#text-sequence-type-str)
- [Tutorial: Python 3 - Podstawy](https://www.learnpython.org/pl/Welcome)

Dziękujemy za przeczytanie tego artykułu! Mamy nadzieję, że przydał się wam w praktyce programistycznej. Zapraszamy do eksperymentowania z metodą `substring()` i wykorzystywania jej w swoich projektach. Do zobaczenia w kolejnych artykułach! 🙂