---
title:    "Ruby: Stora bokstäver i en sträng"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Varför
 
Att skriva kod som är lätt att läsa och förstå är viktigt för alla programmerare. En av de enklaste sätten att göra detta är att följa konventionerna som används av standardspråket. Att kapitalisera en sträng är en av dessa konventioner som kan hjälpa till att göra koden mer läsbar och strukturerad.

## Hur man gör 

```Ruby
# Skapa en variabel med en sträng som inte är kapitaliserad
strang = "hej alla"

# Använd metoden .capitalize för att ändra strängen till att börja med en stor bokstav
kapitaliserad_strang = strang.capitalize

puts kapitaliserad_strang

# Output: "Hej alla"
```

Det är också möjligt att använda metoden .upcase för att helt kapitalisera hela strängen:

```Ruby
strang = "hej alla"

kapitaliserad_strang = strang.upcase

puts kapitaliserad_strang

# Output: "HEJ ALLA"
```

## Djupdykning

När man kapitaliserar en sträng, är det viktigt att förstå att det bara påverkar den första bokstaven i strängen. Om strängen innehåller en mix av stora och små bokstäver, kommer alla andra bokstäver att förbli oförändrade. Det betyder också att om den första bokstaven redan är kapitaliserad, så kommer det inte att göras några förändringar.

Det finns också andra metoder som kan användas för att manipulera strängen, såsom .downcase som helt omvandlar strängen till små bokstäver eller .swapcase som byter ut alla stora bokstäver mot små och vice versa. Det är viktigt att välja den metod som passar dina specifika programmål.

## Se också

- [Ruby dokumentation om strängar](https://ruby-doc.org/core-3.0.0/String.html)
- [Skillshare kurs: Introduction to Ruby Programming](https://www.skillshare.com/classes/Introduction-to-Ruby-Programming/1210676953)
- [Ruby community forum](https://www.ruby-forum.com/)