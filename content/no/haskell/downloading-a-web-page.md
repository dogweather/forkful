---
title:                "Last ned en nettside"
html_title:           "Haskell: Last ned en nettside"
simple_title:         "Last ned en nettside"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Hvorfor
Lurer du på hvordan du kan laste ned en nettside som en fil på din datamaskin? Det kan være mange grunner til å gjøre dette, for eksempel hvis du ønsker å lagre en kopi av en favorittartikkel eller undersøke kildekoden til en side for å lære mer om webutvikling.

# Hvordan gjøre det
Det finnes flere måter å laste ned en nettside på, men vi skal se på hvordan du kan gjøre det ved hjelp av Haskell. Følgende eksempelkoder kan utføres i en Haskell miljø og vil fungere på både Windows, Mac og Linux.

```Haskell
-- Importerer biblioteket for å håndtere HTTP-forespørsler
import Network.HTTP.Simple

-- Setter URLen til nettsiden du ønsker å laste ned
url = "http://www.eksempelnettsted.com"

-- Kobler til nettsiden og lagrer svaret i en variabel
response <- httpLBS url

-- Konverterer svaret til en streng og lagrer det i en fil
let content = getResponseBody response
writeFile "nettside.html" content
```

Dette vil lagre nettsiden som en HTML-fil på ditt filsystem. Du kan tilpasse koden for å endre navn på filen eller lagre den i en spesifikk mappe.

# Dypdykk
Det kan være nyttig å vite at `writeFile` funksjonen vil overskrive eventuelle eksisterende filer med samme navn. Hvis du heller ønsker å legge til nettsiden til en eksisterende fil, kan du bruke `appendFile` funksjonen i stedet.

For å få en bedre forståelse av HTTP-forespørsler i Haskell, kan du sjekke ut følgende ressurser:

- [Dokumentasjon for Network.HTTP.Simple](https://hackage.haskell.org/package/http-client)
- [En enkel guide til HTTP-forespørsler i Haskell](https://www.techempower.com/blog/2013/03/26/everything-you-wanted-to-know-about-http-in-haskell/)

# Se også
- [Offisiell Haskell dokumentasjon](https://www.haskell.org/documentation/)
- [Haskell for nybegynnere](https://wiki.haskell.org/Introduction_to_Haskell)