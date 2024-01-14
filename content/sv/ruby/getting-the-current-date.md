---
title:    "Ruby: Hämta aktuellt datum"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Varför 
Att få den aktuella datumen är en viktig del av programmering i Ruby. Att kunna hämta och manipulera datum är avgörande för att skriva effektiv kod och skapa användarvänliga applikationer. Så om du är en Ruby-utvecklare, kommer du säkert att stöta på behovet att hämta den aktuella datumen i dina projekt.

## Hur man gör det 
För att få den aktuella datumen i Ruby, kan du använda inbyggda metoder som `Time.now`, `Date.today` eller `DateTime.now`. Låt oss titta på ett exempel:

```Ruby
puts "Idag är det #{Date.today}"
```

Detta kodexempel använder `Date.today` för att hämta dagens datum och skriver sedan ut det i terminalen. Resultatet ska se ut så här: 

```
Idag är det 2021-09-10
```

Du kan också använda dessa metoder för att manipulera datum, t.ex. att lägga till eller subtrahera dagar eller veckor. Om du vill ha en mer detaljerad tidsstämpel, kan du använda `DateTime.now`, som ger dig även timmar och minuter. 

```Ruby 
puts "Klockan är just nu #{DateTime.now}"
```

Resultatet kan se ut så här:

```
Klockan är just nu 2021-09-10 10:30:00 +0300
```

## Djupdykning 
För att verkligen förstå hur man hämtar den aktuella datumen i Ruby, är det viktigt att förstå hur Ruby hanterar datum och tid. Ruby har flera olika klasser för datum och tid, såsom `Date`, `Time` och `DateTime`, som alla har olika metoder och funktioner. 

Det aktuella datumet är också beroende av den aktuella tidszonen som din dator är inställd på. Om du vill hämta datumet i en annan tidszon, måste du använda en annan metod, t.ex. `Time.utc` eller `Time.local`, och ange den önskade tidszonen som ett argument. 

Att kunna hantera datum och tid korrekt är viktigt för att undvika misstag i kod, så det är definitivt värt att ta sig tid att lära sig mer om dessa inbyggda metoder och klasser. 

## Se även 
- [Datum och tid i Ruby](https://www.rubyguides.com/ruby-datetime-methods/) 
- [Inbyggda datums-klasser i Ruby](https://ruby-doc.org/stdlib/libdoc/date/rdoc/Date.html) 
- [Hantering av tidzoner i Ruby](https://www.sitepoint.com/managing-time-zones-ruby/)