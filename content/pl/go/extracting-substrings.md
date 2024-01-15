---
title:                "Wyciąganie podciągów"
html_title:           "Go: Wyciąganie podciągów"
simple_title:         "Wyciąganie podciągów"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/extracting-substrings.md"
---

{{< edit_this_page >}}

## Dlaczego

Wyodrębnianie podciągów jest przydatną umiejętnością, którą każdy programista Go powinien posiadać. Dzięki niej możemy w prosty sposób przetwarzać i manipulować łańcuchami znaków w naszych programach.

## Jak To Zrobić

```Go
// Przykładowy łańcuch znaków
s := "Cześć! Witaj w świecie Go!"

// Wyodrębnienie podciągu od indeksu 7 do końca łańcucha
podciag := s[7:]

// Wyświetlenie wyniku
fmt.Println(podciag)

// Output: Witaj w świecie Go!
```

Oto kilka przykładowych kodów, które pokazują, jak wyodrębniać podłańcuchy za pomocą wbudowanych funkcji Go.

* `s[start:]` - wyodrębnia podłańcuch od podanego indeksu do końca łańcucha
* `s[:end]` - wyodrębnia podłańcuch od początku do podanego indeksu
* `s[start:end]` - wyodrębnia podłańcuch od podanego indeksu do innego podanego indeksu

Możemy również wykorzystać funkcję `len` do określenia długości łancucha i wyodrębniać podłańcuchy w oparciu o tę informację.

```Go
// Wyodrębnienie podciągu od indeksu 6 do 10
podciag := s[6:len(s)-4]

// Output: Witaj
```

## Deep Dive

Wyodrębnianie podciągów jest możliwe dzięki temu, że Go traktuje łańcuchy jak tablice bajtów. To pozwala nam korzystać z indeksów, aby operować na poszczególnych znakach w łańcuchu.

Pamiętaj, że indeksowanie w Go zaczyna się od 0, więc pierwszy znak w łańcuchu ma indeks 0, drugi ma indeks 1, itd.

Możesz również wykorzystać operator `range` do iteracji po każdym bajcie w łańcuchu i wyodrębniania podłańcuchów. To przydatne w przypadku, gdy nie znamy dokładnej długości łańcucha.

## Zobacz również
- [Dokumentacja Go o łańcuchach](https://golang.org/doc/effective_go.html#strings)
- [Wideo: Go dla każdego: Manipulowanie łańcuchami](https://youtu.be/mZvKpt7-h_o)
- [Przykłady kodu Go na GitHubie](https://github.com/avelino/awesome-go#strings)