---
title:                "Ruby: Söka och ersätta text"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Varför

Många programmerare behöver ofta söka och byta ut text i sina kodfiler. Det kan vara för att korrigera stavfel, ändra variabelförnamn eller göra mer omfattande ändringar. Men varför är det viktigt att ha goda kunskaper om sökning och utbyte i Ruby? Jo, det sparar tid och minskar risken för felaktiga ändringar i koden.

## Hur man gör det

För att söka och byta ut text i Ruby kan du använda metoden `gsub`. Den tar två argument: en sträng som du vill söka efter och en sträng som du vill ersätta den med. Till exempel:

```Ruby
text = "Hej världen!"
puts text.gsub("Hej", "Hello")
```

Detta kommer att skriva ut "Hello världen!" på skärmen. Du kan också använda reguljära uttryck (regular expressions) för mer avancerad sökning och ersättning. Till exempel:

```Ruby
text = "Det var en gång en katt som hette Kalle."
puts text.gsub(/\b(Kalle)\b/i, "Felix")
```

Detta kommer att ersätta ordet "Kalle" med "Felix" oavsett om det är skrivet med versaler eller gemener.

## Djupdykning

Metoden `gsub` kan ta emot ytterligare ett argument, en block, som ger dig möjlighet att göra ännu mer avancerade ändringar. Till exempel kan du byta ut ett ord med hjälp av ett villkor i blocket. Se nedan:

```Ruby
text = "Jag älskar att programmera i Ruby!"
puts text.gsub("älskar") {|match| match == "älskar" ? "hatar" : "älskar" }
```

Innan du kör koden ovan, försök att förutsäga vad som kommer att skrivas ut på skärmen. Du kommer att bli förvånad över resultatet!

## Se även

- [Ruby String Dokumentation](https://ruby-doc.org/core-2.7.0/String.html)
- [Regex Tutorial för Ruby](https://www.regular-expressions.info/ruby.html)
- [100 utmaningar i Ruby](https://github.com/jorgegonzalez/beginners-challenges#ruby)