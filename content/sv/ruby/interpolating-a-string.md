---
title:                "Interpolera en sträng"
html_title:           "C++: Interpolera en sträng"
simple_title:         "Interpolera en sträng"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/interpolating-a-string.md"
---

{{< edit_this_page >}}

# Stränginterpolation i Ruby

## Vad & Varför?

Stränginterpolation i Ruby är en metod för att infoga variabler direkt i en sträng. Progammare gör detta för att skapa dynamiska strängar utan att behöva använda + operatorn för att lägga ihop delarna.

## Hur gör man:

```Ruby
namn = "Oscar"
puts "Hej, #{namn}!"  # Detta är en interpolerad sträng
```
Som skriver ut: 
```
Hej, Oscar!
```

## Djupdykning

1. **Historisk kontext**: Stränginterpolation har länge använts i olika programmeringsspråk, inklusive de gamla klassikerna som C och Perl.

2. **Alternativ**: Man kan sammanfoga strängar med + operatorn, men det kan bli rörigt om du har många variabler. Till exempel: 
```Ruby
förnamn = "Oscar"
efternamn = "Svensson"
puts "Hej, " + förnamn + " " + efternamn + "!"
```

3. **Implementeringsdetaljer**: Ruby parsa faktiskt din sträng och skapar en helt ny sträng med din interpolerade variabel. Det betyder att varje gång du använder stränginterpolation, skapar Ruby en helt ny sträng.

## Se Även

- [How does string interpolation work in Ruby?](https://stackoverflow.com/questions/10076579/how-does-string-interpolation-work-in-ruby)