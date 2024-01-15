---
title:                "Analysera html"
html_title:           "Go: Analysera html"
simple_title:         "Analysera html"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/parsing-html.md"
---

{{< edit_this_page >}}

## Varför

Om du någonsin har försökt att extrahera data från en webbsida, vet du att det kan vara en tidskrävande och tråkig process att manuellt klicka sig igenom HTML-koden. Genom att använda Go kan du skapa ett program som automatiskt analyserar och extraherar data från HTML-sidor, vilket sparar tid och minskar risken för mänskliga fel.

## Hur man gör

För att parsa HTML i Go, behöver du först importera "html" paketet i ditt program. Sedan kan du använda funktionen "NewTokenizer" för att skapa en tokenizer som kan dela upp HTML-koden i olika tokens (t.ex. starttag, text och sluttag). Du kan sedan använda dessa tokens för att hitta och extrahera önskad data från HTML-koden.

För att visa enkelheten och kraften i att parsa HTML i Go, här är ett exempel på ett program som hämtar och skriver ut titlarna på alla länkar på en webbsida:

```Go
package main

import (
    "fmt"
    "net/http"
    "golang.org/x/net/html"
)

func main() {
    response, _ := http.Get("https://www.example.com")
    tokenizer := html.NewTokenizer(response.Body)

    for {
        token := tokenizer.Next()

        switch {
        case token == html.ErrorToken:
            return
        case token == html.StartTagToken:
            token := tokenizer.Token()
            if token.Data == "a" {
                for _, attr := range token.Attr {
                    if attr.Key == "title" {
                        fmt.Println(attr.Val)
                    }
                }
            }
        }
    }
}
```

Output:

```sh
Example Title 1
Example Title 2
Example Title 3
```

## Djupdykning

I exemplet ovan använder vi funktionen "Next" för att få nästa HTML-token och sedan en switch-sats för att kontrollera vilken typ av token vi fått. Det finns flera olika typer av HTML-tokens som tokenizer kan returnera, och det är viktigt att förstå hur dessa fungerar för att kunna extrahera korrekt data.

En annan viktig del av att parsa HTML är att förstå strukturen och hierarkin i HTML-dokumentet. Genom att använda olika funktioner som "NextSibling" och "FirstChild" kan du navigera genom HTML-trädet och hitta de element du är intresserad av.

Det finns också flera olika attribut som kan användas för att hitta och extrahera data från HTML-koden. Till exempel kan du använda "id" för att hitta ett specifikt element med ett unikt id, eller "class" för att hitta flera element med samma klass.

Generellt sett finns det många detaljer och tekniker för att parsa HTML i Go, men med lite övning kan du skapa robusta och effektiva program som kan hantera olika typer av HTML-kod.

## Se även

Här är några användbara länkar för vidare läsning och lärande om att parsa HTML i Go:

- Golang.org: [Parsa HTML](https://golang.org/pkg/html/)
- Blogg från Eric Chiang: [Hantering av HTML med Go](https://blog.erikwinter.nl/html-handling-with-go/)
- GratisGo.com: [Webb-scraping med Go](https://gratisgo.com/web-scraping-with-go/)