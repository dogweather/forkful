---
title:                "Ruby: Omvandling av en sträng till gemener"
simple_title:         "Omvandling av en sträng till gemener"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Varför

Det finns många olika situationer där det kan vara användbart att konvertera en sträng till gemener, eller lower case som det brukar kallas på engelska. Till exempel när man hanterar användarinput eller när man jämför strängar för att undvika förvirring mellan stora och små bokstäver.

## Så här gör du

För att konvertera en sträng till lower case i Ruby kan man använda sig av metoden `downcase` som finns tillgänglig på alla strängar i språket. Här är ett enkelt exempel:

```Ruby
name = "Lisa"
puts name.downcase
```

Detta skulle resultera i att strängen "Lisa" blir konverterad till "lisa" när den skrivs ut. Detta är bara ett enkelt exempel, men det finns många fler användningsområden för denna metod och den är enkel att använda.

## Deep Dive

När man konverterar en sträng till lower case i Ruby så ändras inte den ursprungliga strängen, utan det returneras istället en ny sträng med den konverterade versionen. Detta beror på att strängar är en immutable, eller oföränderlig, datastruktur i Ruby.

Om man vill ignorera eventuella icke-latiniska tecken på samma sätt som `downcase` gör, så kan man istället använda sig av metoden `unicode_normalize` tillsammans med `downcase`. Detta kan vara användbart om man hanterar text på flera olika språk med olika alfabet.

## Se även

- [Ruby Strings](https://ruby-doc.org/core-2.7.1/String.html)
- [List of Unicode Normalization Forms](https://unicode.org/reports/tr15/)