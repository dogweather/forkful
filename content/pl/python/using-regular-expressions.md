---
title:                "Python: Używanie wyrażeń regularnych"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Dlaczego

Regularne wyrażenia są potężnym narzędziem w świecie programowania. Pozwalają nam wyszukać i manipulować tekstem w sposób szybki i precyzyjny.

## Jak to zrobić 

Przedstawimy przykładowe użycie regularnych wyrażeń w języku Python za pomocą dwóch popularnych funkcji: `search()` i `findall()`. 

```Python
# Przeglądając załączony plik tekstowy
tekst = "Mam na imię Kasia, a ty Jakub?"
# Używamy funkcji search() do wyszukania słowa "Kasia"
znalezione = re.search("Kasia", tekst)
# Funkcja zwraca obiekt typu "Match"
print(znalezione)
# Output: <re.Match object; span=(11, 16), match='Kasia'>
# Używamy funkcji findall() do znalezienia wszystkich słów
# zaczynających się na duże litery
znalezione = re.findall("[A-Z][a-z]*", tekst)
print(znalezione)
# Output: ['Mam', 'Kasia', 'Jakub']

```

## Wnikliwe badanie

Regularne wyrażenia mają szerokie zastosowanie w różnych dziedzinach, takich jak analiza tekstu, weryfikacja danych, a nawet w grach komputerowych. W języku Python, funkcje `search()` i `findall()` są tylko wierzchołkiem góry lodowej, ponieważ istnieje wiele innych funkcji i możliwości. Dodatkowo, regularne wyrażenia posiadają wiele skrótów i specjalnych znaków, które pozwalają na jeszcze dokładniejszą pracę z tekstem. 

## Zobacz też

Jeśli jesteś zainteresowany/a dalszym zgłębieniem wiedzy o regularnych wyrażeniach, polecamy zapoznanie się z poniższymi artykułami: 

- ["Dokumentacja Pythona o regularnych wyrażeniach"](https://docs.python.org/3/library/re.html)
- ["Regularne wyrażenia w praktyce – mini poradnik"](https://realpython.com/regex-python/)
- ["Regularne wyrażenia w grach komputerowych"](https://www.gamedev.net/tutorials/programming/general-and-gameplay-programming/regular-expressions-in-game-development-r4161/)