---
title:                "Skicka en http-begäran"
html_title:           "Go: Skicka en http-begäran"
simple_title:         "Skicka en http-begäran"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Varför

Du kanske har hört talas om "HTTP requests", men varför skulle du vilja använda dem i dina Go-program? En HTTP request är en enkel metod för att kommunicera med en annan server och få tillbaka data eller utföra en åtgärd.

## Hur man gör

För att skicka en HTTP request i Go behöver du först importera paketet "net/http". Sedan kan du använda funktionen "Get" för att göra en GET request till den önskade URL:en.

```Go
import "net/http"

res, err := http.Get("https://www.example.com")
```

Detta skickar en GET request till "https://www.example.com" och sparar resultatet i variabeln "res". Om allt går som planerat kommer "err" att vara "nil" och du kan sedan använda "res" för att läsa det data som returnerats från servern.

```Go
body, err := ioutil.ReadAll(res.Body)

// Läs in den returnerade datan
fmt.Println(string(body))

// Stäng anslutningen
res.Body.Close()
```

I det här exemplet använder vi funktionen "ReadAll" för att läsa all den data som returnerats från servern. Sedan konverterar vi denna data till en sträng och skriver ut den. Slutligen stänger vi anslutningen.

## Deep Dive

Det finns flera olika metoder för att skicka HTTP requests i Go, som "Get", "Post", "Put" och "Delete". Du kan också använda funktionen "NewRequest" för att skicka en mer anpassad request med kundanpassade HTTP headers och body. Det är också möjligt att lägga till timeout-funktioner för att hantera situationer där servern inte svarar.

För att lära dig mer om HTTP requests i Go, kan du kolla in Go's dokumentation om paketet "net/http": https://golang.org/pkg/net/http/

## Se även

- En guide för att lära sig Go på svenska: https://www.learn-golang.org/sv/
- Officiell dokumentation för Go: https://golang.org/doc/
- "Gopher" forum för Go-programmerare: https://forum.golangbridge.org/