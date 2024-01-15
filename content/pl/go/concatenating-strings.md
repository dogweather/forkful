---
title:                "Łączenie ciągów znaków"
html_title:           "Go: Łączenie ciągów znaków"
simple_title:         "Łączenie ciągów znaków"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/concatenating-strings.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek znalazłeś się w sytuacji, w której musiałeś połączyć ze sobą dwa lub więcej łańcuchów znaków? Może chciałeś wyświetlić imię użytkownika w powitaniu lub stworzyć dynamiczny link na stronę internetową. W takich przypadkach bardzo przydatną umiejętnością jest umiejętność łączenia (konkatenacji) stringów. Dzięki temu artykułowi, dowiesz się, jak w łatwy sposób połączyć stringi w języku Go.

## Jak to zrobić 

Do konkatenacji stringów w języku Go możemy użyć operatora "+" lub funkcji "fmt.Sprintf()". Przykładowy kod wyglądałby następująco:

```Go
firstName := "Jan"
lastName := "Kowalski"
fmt.Println("Witaj " + firstName + " " + lastName + "!")
```

lub

```Go
firstName := "Jan"
lastName := "Kowalski"
greeting := fmt.Sprintf("Witaj %s %s!", firstName, lastName)
fmt.Println(greeting)
```

W obydwu przypadkach otrzymalibyśmy taki sam wynik - "Witaj Jan Kowalski!". Warto zauważyć, że funkcja "Sprintf()" umożliwia nam również formatowanie stringów, co może być przydatne w niektórych przypadkach.

## Deep Dive

W języku Go istnieje funkcja wbudowana "strings.Join()", która pozwala na konkatenację wielu stringów w jednym kroku. Przyjmuje ona jako argumenty słice (slice) stringów i oddzielnik, który będzie wstawiany pomiędzy połączonymi elementami. Przykładowy kod wykorzystujący tę funkcję mógłby wyglądać tak:

```Go
names := []string{"Jan", "Anna", "Maria"}
joinedNames := strings.Join(names, ", ")
fmt.Println("Nasi użytkownicy to: " + joinedNames + ".")
```

Wynikiem tego kodu byłoby wyświetlenie "Nasi użytkownicy to: Jan, Anna, Maria.". Funkcja ta jest wydajniejszą opcją w przypadku konkatenacji wielu stringów, ponieważ nie tworzy ona dużych odnośników pamięci na każdy nowy string.

## Zobacz też

Jeśli chcesz dowiedzieć się więcej o funkcjach wbudowanych i wykorzystaniu stringów w języku Go, polecamy przeczytać następujące artykuły:

- https://golang.org/pkg/strings/
- https://tour.golang.org/basics/1