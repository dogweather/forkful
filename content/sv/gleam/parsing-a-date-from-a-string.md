---
title:                "Att tolka ett datum från en sträng"
html_title:           "Gleam: Att tolka ett datum från en sträng"
simple_title:         "Att tolka ett datum från en sträng"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

Gleam: Att parsa ett datum från en sträng

### Vad & Varför?
Att parsa ett datum från en sträng är när man tar en textsträng och omvandlar den till ett datatyp för datum. Programmare gör detta för att kunna hantera datumet och utföra operationer som att jämföra det med andra datum eller visa det på ett annat språk.

### Så här gör du:
I Gleam finns det inbyggda funktioner som gör det enkelt att parsa ett datum från en sträng.
```
Gleam/// let date_string = "12-05-2020" /// let date = Date.parse(date_string, "%d-%m-%Y") /// date //=> 12-05-2020
```
I det här exemplet skapar vi först en textsträng som innehåller ett datum. Sedan använder vi funktionen `Date.parse` för att parsa strängen och få ut ett datum i ett visst format, i det här fallet dd-mm-yyyy. Slutligen skriver vi ut värdet av `date` som i detta fall är 12 maj 2020.

Gleam har också andra inbyggda funktioner som gör det möjligt att parsa datum från olika format, såsom ISO 8601, Unix-timestamp och mer.

### Djupdykning:
Att behandla datum i programmering är en viktig uppgift som ofta kräver en hel del hantering av olika format och tidszoner. Innan inbyggda funktioner som i Gleam fanns, användes ofta tredjepartsbibliotek för att parsa datum på ett enkelt sätt.

Det finns också alternativ till att parsa datum från strängar, som att använda numeriska representationer av datum eller att använda generella formateringsfunktioner för att visa datum i ett specifikt format.

Implementationen av `Date.parse` i Gleam är baserad på funktioner som finns i Elixir och som är influerade av Ruby. Detta gör att den är lätt att använda för de som är bekanta med dessa språk.

### Se även:
- [Gleams dokumentation för Date](https://gleam.run/packages/gleam/gleam_stdlib/0.36.1/Date.html)
- [ISO 8601 - Datumsformat standard](https://www.iso.org/iso-8601-date-and-time-format.html)