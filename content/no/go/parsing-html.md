---
title:                "Analysering av html"
html_title:           "Go: Analysering av html"
simple_title:         "Analysering av html"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/parsing-html.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Parsing av HTML er en viktig del av webutvikling. Det innebærer å analysere og tolke HTML-kode for å gjøre den leselig og forståelig for en datamaskin. Programmerere bruker parsing for å trekke ut informasjon fra nettsider og manipulere den for å lage dynamiske og interaktive nettsider.

## Hvordan:
I Go-versjonen som er den nåværende, kan du bruke pakken "html" for å parse HTML-kode. Her er et eksempel:

```
Go løk := "<h1> Hei, verden! </h1>"
doc, _ := html.Parse(strings.NewReader(htmlStr))
fmt.Println(doc.FirstChild.FirstChild.Data)
```

Konsollen vil skrive ut "Hei, verden!" ved å bruke "FirstChild" til å få tilgang til den første noden av dokumentet og "Data" for å få teksten som ligger inni noden.

## Dykke Dypere:
Parsing av HTML ble utviklet som et svar på behovet for å håndtere den komplekse strukturen til webkoding. Først ble parsing gjort manuelt, men etter hvert som internett vokste, ble det klart at automatisering var nødvendig. Det finnes forskjellige alternativer til å bruke Go for parsing, som for eksempel regex og andre parseringsspråk som Python og Ruby. Men Go er et godt valg fordi det er enkelt, effektivt og har innebygde funksjoner for å håndtere web parsing.

## Se Også:
- Offisiell Go-dokumentasjon for HTML-pakken https://golang.org/pkg/html/
- En enkel og nyttig tutorial om HTML parsing i Go https://www.thepolyglotdeveloper.com/2016/06/parse-html-string-go-programming-language/ 
- En guide for å unngå vanlige feil når man bruker Go for å parse HTML https://medium.com/@codegressive/top-5-gotchas-for-parsing-html-in-go-3fe69db32810