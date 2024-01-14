---
title:                "Gleam: Nedlasting av en nettside"
simple_title:         "Nedlasting av en nettside"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/downloading-a-web-page.md"
---

{{< edit_this_page >}}

##Hvorfor

Så, du er nysgjerrig på hva Gleam har å tilby for å laste ned nettsider? Vel, det er et nyttig verktøy som lar deg hente data fra nettsider i et strukturert format. Kanskje du vil lage en web scraping applikasjon, eller kanskje du bare trenger å få tak i noen data fra en nettside for å bruke i ditt eget prosjekt. Uansett, Gleam har deg dekket!

##Slik gjør du det

Å laste ned en nettside ved hjelp av Gleam er enkelt. Alt du trenger å gjøre er å følge disse trinnene:

1. Importer "HTML" pakken fra Gleam i ditt prosjekt
   ```Gleam
   import html
   ```

2. Bruk "get" funksjonen fra "Html.Downloader" modulen til å hente nettsiden 
   ```Gleam
   Html.Downloader.get("https://www.example.com")
   ```

3. Du kan også angi en brukeragent for å få en spesifikk respons fra nettsiden 
   ```Gleam
   Html.Downloader.get("https://www.example.com", {user_agent: "Gleam Web Scraper"})
   ```

4. Hvis du vil bruke en annen HTTP metode, som POST eller PUT, kan du angi det som en tredje argument 
   ```Gleam
   Html.Downloader.get("https://www.example.com", {}, :post)
   ```

5. Og det er det! Nå har du hentet nettsiden og kan begynne å analysere og behandle dataene.

##Dypere dykk

Gleam sin HTML modul gir deg også muligheten til å behandle responsen fra et nettsted ved å konvertere den til et "Document" objekt. Dette gjør det enklere å navigere og trekke ut spesifikk informasjon fra nettsiden. Du kan også bruke CSS selektorer til å finne spesifike elementer og data på en nettside.

Det er også verdt å merke seg at du kan bruke Gleam sine HTTP funksjoner til å sende forespørsler til nettsider og behandle responsen uten å laste ned hele nettsiden. Dette kan være nyttig hvis du bare trenger å hente en liten del av en nettside.

##Se også

- [HTML dokumentasjon i Gleam](https://gleam.run/modules/Html.html)
- [Eksempler på web scraping med Gleam](https://github.com/gleam-lang/scraper-examples)
- [Eksempelprosjekt for å laste ned nettsider med Gleam](https://github.com/gleam-lang/example-web-scraper)