---
title:    "Ruby: Att hitta längden på en sträng."
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Varför
Att hitta längden på en sträng kan vara en vanlig uppgift när man programmerar i Ruby. Det kan vara användbart när man vill veta hur många tecken en sträng innehåller eller när man behöver jämföra strängar i en algoritm. Oavsett anledning så är det ett viktigt och användbart koncept i Ruby-programmering.

## Så här gör du
Det finns ett enkelt sätt att hitta längden på en sträng i Ruby genom att använda `length` eller `size` metoderna. Här är ett exempel:

```Ruby
strang = "Hej! Det här är en sträng."
puts strang.length # output: 26
puts strang.size # output: 26
```

Som du kan se i exemplet ovan använder vi antingen `length` eller `size` för att hitta längden på en sträng och sedan skriva ut resultatet med `puts`.

Om du vill inkludera mellanslag eller andra specialtecken i din sträng kan du använda `size` metoden. Den räknar tecknen på samma sätt som det visas i strängen medan `length` metoden inte tar hänsyn till mellanslag och specialtecken.

```Ruby
strang = "   Hej!   "
puts strang.length # output: 5
puts strang.size # output: 11
```

Det är också möjligt att hitta längden på en sträng i ett visst omfång med hjälp av `slice` metoden. Ta en titt på exemplet nedan:

```Ruby
strang = "Hej! Det här är en sträng."
puts strang.slice(3..7).length # output: 5
```

## Djupdykning
För att förstå varför `length` och `size` metoder fungerar på detta sätt behöver vi först förstå hur en sträng lagras i minnet i Ruby. I Ruby, liksom i många andra programmeringsspråk, representeras en sträng som en sekvens av tecken eller bytes. På grund av detta kan strängar enkelt hittas genom att räkna antalet tecken eller bytes i en sträng.

Men det är inte alltid så enkelt. Vissa tecken kan ta upp mer än en byte, till exempel icke-engelska tecken eller specialtecken. Detta kan påverka längden på en sträng i Ruby som inte tar hänsyn till dessa specialtecken. Därför används `size` metoden för att räkna antalet bytes i en sträng som visar den riktiga längden på en sträng även med specialtecken.

## Se även
- [Ruby dokumentation för String klassen](https://ruby-doc.org/core-3.0.2/String.html)
- [Skillshare kurs: "Ruby för nybörjare" (på svenska)](https://www.skillshare.com/classes/Ruby-f%C3%B6r-nyb%C3%B6rjare-Steg-f%C3%B6r-steg/1287564023)
- [Ruby on Rails tutorial för svenska läsare](https://guides.rubyonrails.org/getting_started.html)