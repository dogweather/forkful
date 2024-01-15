---
title:                "Användning av reguljära uttryck"
html_title:           "Ruby: Användning av reguljära uttryck"
simple_title:         "Användning av reguljära uttryck"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Varför

Vad är grejen med reguljära uttryck? Varför borde du överväga att använda dem i din Ruby-kod? Jo, reguljära uttryck är ett kraftfullt verktyg för att söka, bearbeta och manipulera textmönster. Det är ett effektivt sätt att utföra komplexa sökningar och ersättningar som annars skulle vara mycket besvärliga att göra manuellt.

## Så här använder du reguljära uttryck i Ruby

För att använda reguljära uttryck i Ruby, behöver du först skapa ett nytt Regexp-objekt med den sträng som innehåller det mönster du vill söka efter. Till exempel, om du vill söka efter alla förnamn som börjar med "A", skulle du skriva:

```Ruby
name_regexp = Regexp.new("^A")
```

För att söka efter en matchning i en sträng, använder du metoden `.match()` på ditt Regexp-objekt och anger den sträng som du vill söka i som argument. Om det finns en matchning, kommer metoden att returnera en `MatchData`-objekt som innehåller information om matchningen, annars kommer den att returnera `nil`. Till exempel:

```Ruby
name = "Anna"
match_data = name_regexp.match(name)
puts match_data[0] # => "A"
```

Du kan också använda reguljära uttryck med metoden `.scan()`, som returnerar en array med alla matchningar i en sträng. Till exempel:

```Ruby
str = "Hello world, goodbye world"
result = str.scan(/world/)
puts result # => ["world", "world"]
```

Med reguljära uttryck kan du också ersätta matchningar i en sträng med ett annat mönster. Använd `.gsub()` och ange matchningen och ersättningsmönstret som argument. Till exempel:

```Ruby
str = "I love chocolate"
new_str = str.gsub(/chocolate/, "pizza")
puts new_str # => "I love pizza"
```

## Djupdykning

Det finns många olika metoder och mönster som kan användas i reguljära uttryck, och det kan ta lite tid att lära sig dem alla. Några användbara mönster är `.` som matchar vilket tecken som helst, `*` som matchar en förekomst av mönstret noll eller flera gånger, och `+` som matchar en förekomst av mönstret en eller flera gånger. Det finns också olika modifierare som du kan använda för att göra dina sökningar mer specifika, som `i` för att ignorera skillnaden mellan stora och små bokstäver och `m` för att söka över flera rader.

Det är också viktigt att förstå att reguljära uttryck är mycket känsliga för specialtecken, såsom parenteser, hakparenteser och backslash. Om du vill matcha ett av dessa tecken måste du använda en backslash före dem för att "escapa" dem och göra det möjligt för dem att matchas.

Om du vill lära dig mer om reguljära uttryck i Ruby, finns det många bra resurser online, inklusive [den officiella Ruby-dokumentationen](https://ruby-doc.org/core-2.7.1/Regexp.html) och [Regexp Cheat Sheet](https://www.rubyinside.com/media/past/2009/1/ruby_cheat.pdf).

## Se även

- [Ruby-dokumentationen för reguljära uttryck](https://ruby-doc.org/core-2.7.1/Regexp.html)
- [Ruby-docs Cheat Sheet för reguljära uttryck](https://www.rubyinside.com/media/past/2009/1/ruby_cheat.pdf)
- [Regex101 - ett onlineverktyg för att testa reguljära uttryck](https://regex101.com/)