---
title:                "Go: Zusammenführen von Zeichenketten"
programming_language: "Go"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/concatenating-strings.md"
---

{{< edit_this_page >}}

## Warum

Die Konkatenation von Strings ist wichtig, um verschiedene Textfragmente zu einem Ganzen zu verbinden. Dies ist in vielen Programmiersprachen, einschließlich Go, eine häufig verwendete Funktion.

## Anleitung

Um Strings in Go zu konkatentieren, gibt es mehrere Möglichkeiten. Im Folgenden sind einige Beispiele aufgelistet:

#### Mit dem `+` Operator

```Go
name := "John"
age := 30
result := name + " is " + age + " years old."

fmt.Println(result)
```

Das Output wird sein:

```
John is 30 years old.
```

#### Mit `fmt.Sprintf()`

```Go
a := "Welcome"
b := "to"
c := "Go"

result := fmt.Sprintf("%s %s %s!", a, b, c)

fmt.Println(result)
```

Das Output wird sein:

```
Welcome to Go!
```

#### Mit `strings.Join()`

```Go
foods := []string{"Pizza", "Burger", "Taco"}
result := strings.Join(foods, ", ")

fmt.Println("I love", result)
```

Das Output wird sein:

```
I love Pizza, Burger, Taco.
```

## Tiefergehende Informationen

Beim Konkatenieren von Strings ist es wichtig zu beachten, dass die Datentypen der einzelnen Variablen zusammenpassen müssen. Andernfalls kann es zu Fehlern führen. Außerdem gibt es in Go die Möglichkeit, Strings mit dem `+=` Operator zu verändern, anstatt eine neue Variable zu erstellen.

## Siehe auch

- [https://tour.golang.org/basics/1](https://tour.golang.org/basics/1)
- [https://gobyexample.com/string-concatenation](https://gobyexample.com/string-concatenation)
- [https://golangdocs.com/concatenate-strings-in-golang](https://golangdocs.com/concatenate-strings-in-golang)