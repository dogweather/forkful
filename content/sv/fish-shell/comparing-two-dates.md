---
title:                "Jämförande av två datum"
html_title:           "Fish Shell: Jämförande av två datum"
simple_title:         "Jämförande av två datum"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Jämföring av två datum är en vanlig uppgift för programmerare. Det innebär helt enkelt att jämföra om två datum är lika, tidigare eller senare än varandra. Programmerare gör detta för att kunna sortera, filtrera eller hantera data baserat på datum.

## Så här gör du:

### Jämföra datum

Du kan enkelt jämföra två datum i Fish Shell med hjälp av kommandot `date` tillsammans med flaggan `-d`. Du kan ange en datumsträng eller en fil som innehåller ett datum. Sedan kan du använda `test` kommandot tillsammans med `-nt` eller `-ot` flaggor för att jämföra de två datum som skapats av `date`.

```Fish Shell
# Jämför två datum som anges som strängar
if test (date -d "2020-10-10") -nt (date -d "2020-10-01") 
     echo "Det andra datumet är tidigare än det första datumet."
end

# Jämför två datum från filer
if test (date -d (head -n 1 file1.txt)) -ot (date -d (head -n 1 file2.txt))
    echo "Datumet från file1 är senare än datumet från file2."
end
```

### Skriva ut datum

En annan vanlig uppgift är att konvertera datum till olika format och skriva ut dem. Detta kan göras i Fish Shell med hjälp av `date` kommandot och flaggan `+%format`.

```Fish Shell
# Skriv ut dagens datum i formatet DD/MM/YYYY
date +%d/%m/%Y
# Output: 12/11/2020

# Skriv ut veckonummer i år
date +%V
# Output: 46
```

## Djupdykning

### Historisk bakgrund

Jämförelse mellan datum har varit en viktig del av programmering sedan tidiga dagar. Även om datorkraft och programvaror har utvecklats, är grundläggande metoder för att jämföra datum fortfarande desamma.

### Alternativ

Det finns andra sätt att jämföra datum i Fish Shell, t.ex. med hjälp av kommandot `cal` eller `awk` programmet. Men `date` kommandot är det vanligaste sättet att göra det.

### Implementation detaljer

`date` kommandot i Fish Shell gör användning av operativsystemets C bibliotek för att konvertera datum till en Unix timestamp, som sedan kan jämföras med hjälp av `test` kommandot.

## Se även

- [Fish Shell dokumentation](https://fishshell.com/docs/current/index.html)
- [Test kommandot](https://fishshell.com/docs/current/commands.html#test)