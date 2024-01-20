---
title:                "Usuwanie znaków pasujących do wzorca"
html_title:           "C: Usuwanie znaków pasujących do wzorca"
simple_title:         "Usuwanie znaków pasujących do wzorca"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Usuwanie znaków pasujących do wzorca to operacja, która pozwala programistom usunąć konkretne sekwencje znaków z tekstu. Robimy to, aby oczyścić dane wejściowe, usunąć niepożądane znaki lub uprościć przetwarzanie tekstu.

## Jak to zrobić:
Poniżej przykładowe kodowanie i efekty ich działania. 
```PowerShell
# Przykład 1:
$tekst = "Hello, World!"
$wzorzec = "[oie]"
$tekst -replace $wzorzec, '' 
# Wynik: "Hll, Wrld!"

# Przykład 2:
$tekst = "Poniedziałek, środa, piątek"
$wzorzec = "[aeio]"
$tekst -replace $wzorzec, '' 
# Wynik: "Pndłk, śrd, pątk"
```
Powyższe skrypty demonstrują, jak w PowerShell można usuwać znaki pasujące do wzorca.


## Głębsze spojrzenie:
Usuwanie znaków pasujących do wzorca ma swoje korzenie w wyrażeniach regularnych, narzędziu wykorzystywanym do manipulacji tekstem od lat 60-tych XX wieku. Alternatywnie do `-replace`, możemy również użyć metody `.Remove()` dla obiektów string w PowerShell. Jej wadą jest to, że nie obsługuje wzorców, co oznacza, że możemy usunąć tylko konkretne znaki lub sekwencje, a nie wzorce.

```PowerShell
# Przykład 3:
$tekst = "Hello, World!"
$tekst.Remove(5) 
# Wynik: "Hello"
```
Powyższy skrypt usuwa wszystko po piątym znaku.

## Zobacz także:
1. [PowerShell - Poradnik Microsoft](https://docs.microsoft.com/pl-pl/powershell/)
2. [Wyrażenia regularne - Wikipedia](https://pl.wikipedia.org/wiki/Wyra%C5%BCenie_regularne)
3. [PowerShell remove characters in string - Stackoverflow](https://stackoverflow.com/questions/19487800/how-to-remove-characters-in-a-string-using-powershell)