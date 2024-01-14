---
title:                "Go: Odczytywanie kodu html"
simple_title:         "Odczytywanie kodu html"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/parsing-html.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli jesteś programistą, który zaczyna przygodę z językiem programowania Go, prawdopodobnie słyszałeś już o parsowaniu HTML. Jest to niezwykle ważna umiejętność, która pozwala na ekstrakcję danych z kodu HTML, używając prostych i wydajnych narzędzi dostępnych w języku Go. W tym wpisie dowiesz się dlaczego warto nauczyć się parsowania HTML oraz jak możesz to zrobić w prosty sposób.

## Jak to zrobić

Aby zacząć parsować HTML w języku Go, musisz najpierw skonfigurować odpowiednie narzędzia. W tym celu możesz wykorzystać popularną bibliotekę [goquery](https://github.com/PuerkitoBio/goquery), która udostępnia intuicyjne API do przeszukiwania i manipulowania kodem HTML. Poniżej przedstawiamy przykładowy kod wykorzystujący bibliotekę goquery.

```Go
package main

import (
    "fmt"
    "log"
    "strings"

    "github.com/PuerkitoBio/goquery"
)

func main() {
    // Pobranie kodu HTML strony
    doc, err := goquery.NewDocument("https://www.example.com")
    if err != nil {
        log.Fatal(err)
    }

    // Wyszukanie i wyświetlenie tytułu strony
    title := doc.Find("title").Text()
    fmt.Println("Tytuł strony: ", strings.TrimSpace(title))
}
```

Powyższy kod pobiera kod HTML ze strony internetowej i wyszukuje w nim tytuł, który jest następnie wyświetlany w konsoli. Dzięki prostemu interfejsowi biblioteki goquery, możesz łatwo wyszukiwać i manipulować kodem HTML, aby wyodrębnić potrzebne dane.

## Głębsza analiza

Parsowanie HTML w języku Go nie jest tylko ograniczone do wyszukiwania i manipulacji elementami na stronie internetowej. Możesz również wykorzystać dodatkowe narzędzia, takie jak [net/html](https://github.com/golang/net/tree/master/html) do analizowania struktury drzewa DOM i wyodrębniania danych z konkretnych tagów lub atrybutów HTML. Pozwala to na bardziej precyzyjne i zaawansowane przetwarzanie kodu HTML w Twoich aplikacjach.

## Zobacz również

- Oficjalny podręcznik języka Go do parsowania HTML: https://golang.org/pkg/html
- Poradnik poświęcony parsowaniu HTML w języku Go: https://dev.to/dlsniper/parsing-html-in-go-1jnn