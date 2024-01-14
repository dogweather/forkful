---
title:                "Go: Laczenie stringow"
programming_language: "Go"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/concatenating-strings.md"
---

{{< edit_this_page >}}

## Dlaczego

Concatenation jest jedną z podstawowych operacji w programowaniu. W języku Go, łączenie ciągów (ang. strings) jest bardzo proste i wydajne. W tym artykule dowiesz się dlaczego warto użyć tej operacji oraz jak wykorzystać ją w swoich projektach.

## Jak to zrobić

Aby skonkatenować (połączyć) dwa ciągi tekstowe w języku Go, możemy skorzystać z operatora `+` lub funkcji `fmt.Sprint()`.

```Go
str1 := "Hello"
str2 := "World"

// Użycie operatora +
result := str1 + " " + str2

// Użycie funkcji fmt.Sprint()
result := fmt.Sprint(str1, " ", str2)

fmt.Println(result)
// Output: Hello World
```

Możemy również skorzystać z funkcji `strings.Join()`, która pozwala na połączenie wielu ciągów w jednym wywołaniu.

```Go
str1 := "Go"
str2 := "Programming"
str3 := "Language"

// Użycie funkcji strings.Join()
result := strings.Join([]string{str1, str2, str3}, " ")

fmt.Println(result)
// Output: Go Programming Language
```

## Deep Dive

W języku Go, operacja concatenacji jest bardzo wydajna dzięki temu, że ciągi są niemutowalne (niezmienne) i wewnętrznie reprezentowane jako tablice bajtów. Dzięki temu unikamy potrzeby tworzenia nowych ciągów przy każdej operacji concatenacji.

Istnieje również wiele funkcji w pakiecie `strings`, które pozwalają na manipulację i porównywanie ciągów tekstowych, co sprawia, że język Go jest bardzo przyjazny dla programistów pracujących z tekstami.

## Zobacz również

- [Dokumentacja języka Go](https://golang.org/doc/)
- [Tutorial: Rozpoczęcie pracy z językiem Go](https://www.digitalocean.com/community/tutorials/how-to-install-go-and-set-up-a-local-programming-environment-on-ubuntu-18-04)
- [Oficjalny blog języka Go](https://blog.golang.org/)