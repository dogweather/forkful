---
title:    "Python: Znajdowanie długości ciągu znaków"
keywords: ["Python"]
---

{{< edit_this_page >}}

##Dlaczego

Ile razy zdarzyło Ci się dostać tekst od użytkownika i musieć sprawdzić jego długość? Czy wiesz, że istnieje prosty sposób, aby to zrobić w języku Python? Poznaj krótką i łatwą technikę znajdywania długości łańcucha znaków w Pythonie.

##Jak to zrobić?

```Python
# Stwórz zmienną 'tekst' z przykładowym łańcuchem znaków
tekst = "To jest przykładowy tekst"

# Wykorzystaj funkcję len(), aby znaleźć długość tego tekstu
dlugosc = len(tekst)

# Wyświetl długość tekstu na ekranie, używając metody print()
print("Długość tekstu to:", dlugosc)
```

Output:
```
Długość tekstu to: 23
```

Jak widać w przykładzie powyżej, sposób na znalezienie długości łańcucha znaków jest bardzo prosty. W tym przypadku, wykorzystaliśmy wbudowaną funkcję len(), która zwraca ilość elementów w danym obiekcie. W przypadku tekstu, zwraca ona ilość liter.

##Pogłębione omówienie

Funkcja len() może nie tylko być użyta do znalezienia długości łańcucha znaków, ale także do innych typów danych, takich jak listy, krotki czy słowniki. W przypadku listy, zwraca ona ilość elementów w liście, zaś w słowniku - ilość par klucz-wartość.

Warto zaznaczyć, że funkcja len() jest bardzo przydatna w przypadku pętli i iteracji po danym obiekcie. Można wtedy wykorzystać jej wynik jako warunek do zakończenia pętli.

Dzięki temu prostemu sposobowi, można szybko i sprawnie sprawdzać długość różnych typów danych w Pythonie.

##Zobacz również

- [Dokumentacja Pythona o funkcji len()](https://docs.python.org/3/library/functions.html#len)
- [Poradnik dla początkujących w Pythonie](https://realpython.com/python-beginner-tips/)