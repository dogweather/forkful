---
title:    "Go: Łączenie ciągów znaków"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Dlaczego

Często w programowaniu istnieje potrzeba łączenia różnych ciągów znaków w jednym. Dzięki temu możemy tworzyć czytelne teksty lub generować dynamiczne wiadomości. W języku Go istnieje prosty sposób na łączenie stringów, o którym opowiemy w tym artykule.

## Jak to zrobić

Do łączenia stringów w Go używa się operatora `+`. Musimy jednak pamiętać o tym, że w Go ciągi znaków są niemutowalne, więc operacja łączenia tworzy nowy string, a nie zmienia wartości istniejącego. Przykładowe użycie operatora przedstawia poniższy kod:

```Go
message := "Witaj"
name := "Jan"
greeting := message + " " + name
fmt.Println(greeting)
```
Output: `Witaj Jan`

Zauważmy, że każdy string musimy oddzielić spacją, aby uzyskać czytelny tekst.

## Deep Dive

Warto zauważyć, że łączenie wielu ciągów znaków może być czasochłonne w przypadku dużych ilości danych. W takiej sytuacji lepszym rozwiązaniem jest użycie funkcji `strings.Join`, która pozwala na łączenie kilku stringów jednocześnie. Przykładowe użycie tej funkcji prezentuje się następująco:

```Go
names := []string{"Adam", "Ewa", "Paweł"}
greetings := strings.Join(names, ", ")
fmt.Println(greetings)
```

Output: `Adam, Ewa, Paweł`

## Zobacz również

- [Dokumentacja języka Go o ciągach znaków](https://golang.org/pkg/strings/#Join)
- [Przykładowy poradnik na temat manipulacji stringami w Go](https://blog.alexellis.io/golang-string-handling/)
- [Kurs języka Go dla początkujących](https://www.freecodecamp.org/news/learn-go-programming-language-from-scratch-golang-1/)