---
title:                "Go: Sända en HTTP-förfrågan med grundläggande autentisering"
simple_title:         "Sända en HTTP-förfrågan med grundläggande autentisering"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Varför

I den här bloggposten kommer vi att titta på hur man kan skicka HTTP-förfrågningar med grundläggande autentisering i Go-programmering. Detta är en viktig del av att bygga webbapplikationer och API:er, och med hjälp av basic authentication kan du säkerställa att endast auktoriserade användare har tillgång till dina resurser.

## Så här gör du

För att skicka en HTTP-förfrågan med grundläggande autentisering i Go, behöver du först bygga en `http.Client` med hjälp av `http.NewRequest()`-funktionen:

```Go
// Skapa en http.Client
client := &http.Client{}
// Skapa en HTTP-förfrågan
req, err := http.NewRequest("GET", "https://www.example.com", nil)
```

För att lägga till autentiseringsuppgifter i HTTP-förfrågan kan vi använda `SetBasicAuth()`-funktionen:

```Go
// Lägg till autentiseringsuppgifter
req.SetBasicAuth("användarnamn", "lösenord")
```

Sedan kan vi skicka förfrågan och hämta resultatet:

```Go
// Skicka HTTP-förfrågan
resp, err := client.Do(req)
if err != nil {
    panic(err)
}
// Hämta resultatet
defer resp.Body.Close()
body, err := ioutil.ReadAll(resp.Body)

fmt.Println(string(body)) // Skriver ut resultatet av förfrågan
```

Om autentiseringen lyckades kommer du att få ett 200-status (OK). Om inte, kommer du att få ett 401-status (Obehörig). Detta beror på vilken typ av autentisering som ditt API eller webbplats kräver.

## Deep Dive

Det finns många olika typer av autentisering, men vi har valt att fokusera på grundläggande autentisering i den här bloggposten. Det är enkelt att implementera och fungerar bra för de flesta användningsfall.

För att skicka en HTTP-förfrågan med grundläggande autentisering, behöver vi bara lägga till en HTTP-header med namnet "Authorization" i vår förfrågan. Dess värde är baserat på användarnamn och lösenord, och kodas vanligtvis i Base64-format.

Det är också viktigt att notera att grundläggande autentisering endast krypterar användarnamn och lösenord i en Base64-kodad sträng och inte är en säker form av autentisering. För att se till att dina användare och dina resurser är skyddade bör du överväga att använda en mer robust autentisering, som OAuth eller API-nycklar.

Det är också möjligt att skicka HTTP-förfrågningar med grundläggande autentisering i andra programmeringsspråk, som Java eller Python. Det viktigaste är att förstå grundläggande autentiseringens koncept och hur man implementerar det i ditt specifika program.

## Se även

Här är några resurser som kan vara till hjälp för dig när du arbetar med HTTP-förfrågningar med grundläggande autentisering i Go:

- [Officiell dokumentation för Go HTTP-paketet](https://golang.org/pkg/net/http/)
- [Base64-kodning i Go](https://golang.org/pkg/encoding/base64/)
- [En guide för att lära sig mer om HTTP-förfrågningar](https://www.digitalocean.com/community/tutorials/an-introduction-to-http)

Det är också en bra idé att titta på dokumentationen och exempelkod från de API:er eller webbplatser som du planerar att använda grundläggande autentisering med. Var noga med att förstå deras specifika implementation och krav för autentisering.

Tack för att du läste! Förhoppningsvis har du nu en bättre förståelse för hur man skickar HTTP-förfrågningar med grundläggande autentisering i Go. Lycka till