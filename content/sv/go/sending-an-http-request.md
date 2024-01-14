---
title:                "Go: Sända en http-begäran"
simple_title:         "Sända en http-begäran"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Varför

Att skicka HTTP-förfrågningar är en viktig del av webbutveckling. Det tillåter kommunikation mellan olika servrar och används ofta för att hämta data från en webbplats eller API. I Go-programmering kan detta göras på ett enkelt och effektivt sätt genom att använda standardbiblioteket för HTTP. Låt oss ta en titt på hur man kan göra det.

# Hur man gör

För att skicka en HTTP-förfrågan i Go, först måste vi importera paketet som hanterar HTTP-kommunikation. Detta görs genom att använda följande rad kod:

```Go
import "net/http"
```

Nästa steg är att skapa en förfrågan med önskad metod, URL och eventuell data som ska skickas med förfrågan. Detta kan göras på följande sätt:

```Go
req, err := http.NewRequest("GET", "https://example.com/api/users", nil)
if err != nil {
    // hantera fel
}
```

I detta exempel skapas en GET-förfrågan till https://example.com/api/users och ingen data skickas med. Om du vill skicka data kan detta läggas till som ett tredje argument i NewRequest-funktionen.

Nästa steg är att skicka förfrågan och hantera svaret. Detta görs genom att använda Client från http-paketet. Här är ett exempel:

```Go
resp, err := http.DefaultClient.Do(req)
if err != nil {
    // hantera fel
}
defer resp.Body.Close()
```

I detta exempel använder vi DefaultClient för att skapa och skicka en förfrågan. Vi använder också en defer-sats för att se till att kroppen av svarsanropet stängs efter att vi har använt den.

Nu kan vi läsa svaret från vår förfrågan. I detta nästa exempel använder vi ioutil-paketet för att läsa och skriva svaret från en HTTP-förfrågan:

```Go
body, err := ioutil.ReadAll(resp.Body)
if err != nil {
    // hantera fel
}
fmt.Println(string(body))
```

Detta är en enkel kod som läser och skriver ut svaret från vår förfrågan. Det finns många olika funktioner och metoder som kan användas för att hantera HTTP-förfrågningar i Go, men dessa grundläggande exempel bör ge en bra grund.

# Djupdykning

Skicka en HTTP-förfrågan i Go kan verka enkelt, men det finns mycket under ytan som kan utforskas. Till exempel kan du hantera fel vid förfrågan eller använda olika metoder som POST eller PUT för att skicka data till en server. Det finns också möjligheten att anpassa förfrågan ytterligare genom att lägga till HTTP-huvuden eller ändra förfrågans timeout. Det finns många resurser tillgängliga online som kan hjälpa dig att utforska dessa koncept ytterligare.

# Se även

- [Official Go documentation for HTTP](https://golang.org/pkg/net/http/)
- [A tutorial on handling HTTP requests in Go](https://tutorialedge.net/golang/creating-simple-web-server-with-golang/)
- [A deep dive into HTTP in Go](https://medium.com/@dhanushgopinath/deep-dive-into-http-in-golang-5120b56bae44)