---
title:    "Ruby: Konvertere en dato til en streng"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Hvorfor

 Å konvertere en dato til en streng er en viktig del av programmering når man jobber med datoer og tid. Dette gjør det enklere å manipulere og vise datoer på en måte som er forståelig for brukeren.

## Slik gjør du det

For å konvertere en dato til en streng i Ruby, kan du bruke metoden `strftime`. Denne metoden tar inn en `DateTime`-objekt og et format som argumenter, og returnerer en streng med den konverterte datoen.

```Ruby
# Oppretter et DateTime-objekt for 25. desember 2020
dato = DateTime.new(2020, 12, 25)

# Konverterer datoen til en streng i formatet dd/mm/yyyy
puts dato.strftime("%d/%m/%Y")
# Output: 25/12/2020
```

I eksempelet over bruker vi `%d`, `%m` og `%Y` for å representere dag, måned og år i strengen. Det finnes flere formateringsmuligheter, avhengig av hva du ønsker å vise i strengen. Du kan blant annet legge til klokkeslett, tidssone og ukedag.

## Dykk dypere

For å få en full forståelse av hvordan `strftime` fungerer, kan det være nyttig å se hvordan det håndterer forskjellige formatstrenger. Hvis du for eksempel ønsker å vise måneden på tre bokstaver i stedet for tall, kan du bruke `%b` i formatstrengen. Her er noen eksempler på ulike formatstrenger og hva de vil vise:

- `%d`: Dag i måned (01-31)
- `%m`: Måned (01-12)
- `%b`: Måned som en forkortelse (Jan, Feb, Mar)
- `%Y`: År (2020)
- `%H`: Timer i 24-timers format (00-23)
- `%M`: Minutter (00-59)
- `%S`: Sekunder (00-59)
- `%a`: Ukedag som en forkortelse (Mon, Tue, Wed)
- `%j`: Dag i året (001-366)

Ved å blande og matche disse formateringsmulighetene, kan du lage en streng som viser akkurat den informasjonen du ønsker å vise.

## Se også

- [DateTime klasse i Ruby dokumentasjonen](https://ruby-doc.org/stdlib-2.7.1/libdoc/date/rdoc/DateTime.html)
- [Ruby DateTime klasse tutorial](https://www.tutorialspoint.com/ruby/ruby_date_time.htm)
- [Date and Time metoder i Ruby](https://www.geeksforgeeks.org/date-and-time-methods-in-ruby/)