---
title:                "Omvandla en sträng till gemener"
html_title:           "Ruby: Omvandla en sträng till gemener"
simple_title:         "Omvandla en sträng till gemener"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Varför

Att konvertera en sträng till små bokstäver kan vara användbart för flera olika ändamål. Det kan hjälpa till att standardisera data, jämföra strängar eller göra sökningar enklare.

## Såhär

För att konvertera en sträng till små bokstäver i Ruby, kan du använda metoden `downcase`. Här är ett exempel:

```Ruby
exempel = "HEJ DÄR"
puts exempel.downcase 
```

Output:

```
hej där
```

Om det finns icke-bokstavliga tecken i strängen, så kommer de att förbli oförändrade efter konverteringen. Om du vill ta bort dessa tecken kan du istället använda metoden `gsub` tillsammans med en reguljär uttrycksmall.

### En djupdykning

När en sträng konverteras till små bokstäver i Ruby, så används Unicode-teckenuppsättningen för att avgöra vilka bokstäver som ska konverteras. Unicode är en standard för att representera olika skriftsystem och tecken i digital text, och är det som tillåter oss att använda olika språk på samma dator utan problem.

Det finns även en mer aggressiv metod för att konvertera strängar till små bokstäver, som även tar hänsyn till regionala skillnader i bokstavsmönster. Den heter `Unicode::CaseFold` och är en del av standardbiblioteket för Ruby. Det är dock viktigt att notera att denna metod är mindre effektiv och inte nödvändigtvis lämplig i alla situationer.

## Se även

- [Ruby dokumentation om metoden downcase](https://ruby-doc.org/core-3.0.2/String.html#method-i-downcase)
- [Mer information om Unicode](https://www.utf8-chartable.de)