---
title:                "Nedladdning av en webbsida"
html_title:           "Go: Nedladdning av en webbsida"
simple_title:         "Nedladdning av en webbsida"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Varför
Kanske har du stött på en intressant webbsida som du vill spara för senare, eller så vill du kanske automatisera en uppgift som involverar hämtning av webbinnehåll. Oavsett anledning finns det ett enkelt sätt att ladda ner en webbsida i Go.

## Så här gör du
Först måste vi importera "net/http" paketet för att kunna hämta webbsidans innehåll. Sedan använder vi funktionen "Get" från detta paket och anger den URL vi vill hämta som argument. Här är ett enkelt exempel: 

```Go
package main

import (
    "fmt"
    "net/http"
)

func main() {
    response, err := http.Get("https://www.example.com")
    if err != nil {
        // Om det uppstår ett fel, hantera det här
    }
    defer response.Body.Close() // Säkerställ att vi stänger anslutningen när vi är klara
    fmt.Println(response.StatusCode) // Skriv ut statuskoden från webbsvaret
}
```
Output: 200

I detta exempel använder vi variabeln "response" för att lagra det svar vi får tillbaka från "Get" funktionen. Sedan använder vi "defer" för att stänga anslutningen till webbsidan när vi är klara. Slutligen skriver vi ut statuskoden från svaret.

## Djupdykning
Som du kan se i exemplet ovan så är det faktiskt ganska enkelt att hämta en webbsida i Go. Men det finns fler saker vi kan göra för att anpassa vår hämtning. Till exempel kan vi använda "Body" attributet från "response" variabeln för att få tillgång till själva webbinnehållet. Vi kan också använda paketet "io/ioutil" för att enkelt läsa innehållet som en sträng.

```Go
package main

import (
    "fmt"
    "net/http"
    "io/ioutil"
)

func main() {
    response, err := http.Get("https://www.example.com")
    if err != nil {
        // Om det uppstår ett fel, hantera det här
    }
    defer response.Body.Close()
    body, err := ioutil.ReadAll(response.Body)
    if err != nil {
        // Om det uppstår ett fel, hantera det här
    }
    fmt.Println(string(body)) // Skriv ut webbinnehållet som en sträng
}
```
Output: <!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
</html>

I detta exempel använder vi paketet "io/ioutil" för att läsa innehållet från "Body" attributet som en sträng och skriva ut det. Detta kan vara användbart om du vill bearbeta innehållet på något sätt, som att kanske parsea HTML eller spara det till en fil.

## Se även
- [Officiell dokumentation för "net/http" paketet](https://golang.org/pkg/net/http/)
- [Mer om "io/ioutil" paketet](https://golang.org/pkg/io/ioutil/)
- [En praktiskt exempel på webb-scarping i Go](https://levelup.gitconnected.com/web-scraping-in-go-c40a76572b2e?gi=6f6f4754817)