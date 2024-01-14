---
title:    "Fish Shell: Ekstrakcja podciągów"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

# Dlaczego?

W programowaniu często używamy funkcji do manipulacji łańcuchami tekstu, a wyodrębnianie podłańcuchów jest jednym z kluczowych procesów. Pozwala nam to na efektywne i precyzyjne zarządzanie tekstem, bez konieczności przeszukiwania go w całości. W tym wpisie dowiesz się, jak wyodrębnić podłańcuchy za pomocą języka programowania Fish Shell.

# Jak to zrobić?

Wyodrębnienie podłańcuchów w Fish Shell jest bardzo proste. Wystarczy użyć wbudowanej funkcji ```string sub```, która przyjmuje dwa argumenty: pierwotny łańcuch oraz indeks początkowy i końcowy podłańcucha, który chcemy wydzielić. Poniżej znajduje się przykładowy kod:

```
Fish Shell: string sub "Hello World" 6 10

Output: World
```

W powyższym przykładzie wyodrębniliśmy podłańcuch "World" z łańcucha "Hello World", zaczynając od indeksu 6 (licząc od zera) i kończąc na indeksie 10 (bez niego). Oznacza to, że wyodrębniony zostanie łańcuch od 6-tego do 10-tego indeksu.

Możemy także wyodrębniać podłańcuchy korzystając z indeksów ujemnych. Jeśli podamy indeks ujemny, to będzie on liczył się od końca łańcucha. Na przykład, jeśli chcemy wyodrębnić ostatnie 5 znaków z łańcucha, możemy użyć takiego kodu:

```
Fish Shell: string sub "abcdefghijklmnopqrstuvwxyz" -5 -1

Output: vwxyz
```

Zwróć uwagę na indeks końcowy -1. Jest to spowodowane tym, że indeksy w języku Fish Shell są zawsze o 1 mniejsze od faktycznej liczby znaków. Możemy temu zapobiec, dodając argument "length" do naszej funkcji, który określa długość wyodrębnionego łańcucha. Przykładowo:

```
Fish Shell: string sub "abcdefghijklmnopqrstuvwxyz" -5 -1 length 5

Output: vwxyz
```

# Deep Dive

Funkcja ```string sub``` pozwala nam także na wyodrębnianie podłańcuchów z podłańcuchów, tworząc w ten sposób bardziej złożone operacje. Możemy również wykorzystać ją w innych funkcjach, np. w pętlach lub warunkach, aby bardziej zaawansowanie manipulować tekstem.

Poniżej znajdują się przykładowe linki z dokumentacją i kodami, które mogą Ci się przydać w wyodrębnianiu podłańcuchów w Fish Shell:

## Zobacz również

- Oficjalna dokumentacja Fish Shell: https://fishshell.com/docs/current/commands.html#string-sub
- Przykładowe kody z wykorzystaniem funkcji ```string sub```: https://github.com/fish-shell/fish-shell/blob/master/share/functions/string_sub.fish