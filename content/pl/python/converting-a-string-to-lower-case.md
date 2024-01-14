---
title:    "Python: Konwersja ciągu znaków na małe litery"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Dlaczego

Wielu programistów często potrzebuje zamienić tekst na małe litery w celu ułatwienia porównywania i przetwarzania danych. Na przykład, gdy użytkownik wprowadza dane do formularza, można użyć funkcji zamieniającej tekst na małe litery, aby zapobiec błędom związanym z wielkością liter.

## Jak to zrobić

Konwersja tekstu na małe litery w języku Python jest bardzo prosta i może być wykonana za pomocą wbudowanej funkcji ```lower()```. Poniżej znajdują się kilka przykładowych kodów, pokazujących, jak użyć tej funkcji:

```Python
# Przykładowy tekst
text = "HELLO WORLD"

# Przykład 1 - użycie funkcji lower() na całym tekście
print(text.lower())
# Output: hello world

# Przykład 2 - użycie funkcji lower() na pojedynczym znaku
print(text[3].lower())
# Output: l

# Przykład 3 - użycie funkcji lower() na liście słów
words = ["PYTHON", "PROGRAMMING", "BLOG"]
print([word.lower() for word in words])
# Output: ["python", "programming", "blog"]
```

## Deep Dive

Podczas pracy z tekstem należy pamiętać, że funkcja ```lower()``` zwraca nowy obiekt, więc oryginalny tekst pozostanie niezmieniony. Istnieje również wiele innych sposobów na konwersję tekstu na małe litery w Pythonie, na przykład można użyć metody ```casefold()``` lub modułu ```string```. Warto również zwrócić uwagę na różnice w konwersji zależnie od języka, ponieważ niektóre litery mogą być różnie przekształcane na małe litery w różnych językach.

## Zobacz również

- [Dokumentacja Pythona - lower()](https://docs.python.org/3/library/stdtypes.html#str.lower)
- [W3Schools - Python String lower() Method](https://www.w3schools.com/python/ref_string_lower.asp)
- [Real Python - Lowercasing Strings in Python](https://realpython.com/python-string-case/)