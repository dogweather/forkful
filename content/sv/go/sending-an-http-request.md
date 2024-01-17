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

# Vad & Varför?
### Vad är en HTTP-request?
När en programmerare vill skicka en förfrågan till en server för att få tillbaka data eller utföra en handling, använder de sig av något som kallas en HTTP-request.

### Varför gör programmerare det här?
HTTP-requests är en viktig del av webbutveckling och används för att kommunicera med olika servrar och tjänster. Genom att skicka en HTTP-request kan en programmerare hämta information från en API eller utföra en handling på en server.

# Hur gör man?
### Kodexempel:
Go erbjuder ett inbyggt paket "net/http" som tillhandahåller ett enkelt sätt att skicka HTTP-requests. Här är ett exempel på hur man kan använda detta paket för att hämta information från en API och skriva ut svaret i terminalen:
```
package main

import (
   "fmt"
   "net/http"
   "io/ioutil"
)

func main() {
   response, err := http.Get("https://swapi.dev/api/people/1") // skickar en GET-request till Star Wars API
   if err != nil {
      fmt.Println(err)
      return
   }

   defer response.Body.Close()

   data, err := ioutil.ReadAll(response.Body)
   if err != nil {
      fmt.Println(err)
      return
   }

   fmt.Println(string(data)) // skriver ut svaret i terminalen
}
```
Output:
```
{
   "name": "Luke Skywalker",
   "height": "172",
   "mass": "77",
   "hair_color": "blond",
   "skin_color": "fair",
   "eye_color": "blue",
   "birth_year": "19BBY",
   "gender": "male",
   "homeworld": "https://swapi.dev/api/planets/1/",
   "films": [
       "https://swapi.dev/api/films/2/",
       "https://swapi.dev/api/films/6/",
       "https://swapi.dev/api/films/3/",
       "https://swapi.dev/api/films/1/",
       "https://swapi.dev/api/films/7/"
   ],
   "species": [
       "https://swapi.dev/api/species/1/"
   ],
   "vehicles": [
       "https://swapi.dev/api/vehicles"
   ],
   "starships": [
       "https://swapi.dev/api/starships/12/",
       "https://swapi.dev/api/starships/22/"
   ],
   "created": "2014-12-09T13:50:51.644000Z",
   "edited": "2014-12-20T21:17:56.891000Z",
   "url": "https://swapi.dev/api/people/1/"
}
```

# Djupdykning
### Historisk kontext:
HTTP-protokollet som används för att skicka dessa requests har funnits sedan 1991 och är en av de grundläggande protokollen för webben. Det har gått igenom flera versioner, den nuvarande är HTTP/2 som introducerades 2015.

### Alternativ:
Det finns flera olika alternativ för att skicka HTTP-requests, bland annat "curl" och "wget". Men iGo's inbyggda "net/http" paket är ett populärt val för dess enkelhet och funktionalitet.

### Implementation:
När man skickar en HTTP-request, använder man sig av olika HTTP-metoder såsom GET, POST, PUT och DELETE för att ange vilken typ av handling som ska utföras på servern. Man kan också skicka olika parametrar och data tillsammans med requesten för att anpassa den ytterligare.

# Se även
- [net/http paketet](https://golang.org/pkg/net/http/)
- [En guide för att lära sig Go](https://tour.golang.org/welcome)