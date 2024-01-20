---
title:                "Omvandla en sträng till gemener"
html_title:           "Arduino: Omvandla en sträng till gemener"
simple_title:         "Omvandla en sträng till gemener"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att omvandla en sträng till gemener (lower case) innebär att ändra samtliga stora bokstäver i en given sträng till små bokstäver. Programmerare gör detta för att undvika oönskad känslighet för skiftläge vid jämförelse av strängar.

## Hur man gör:

Ruby har en inbyggd metod `.downcase` för att omvandla en sträng till gemener. Det är enkelt och snabbt!

```Ruby
str = "HeJ DÄR, SwEdeN!"
puts str.downcase
```

Ovanstående kod omvandlar varje stor bokstav i strängen till en liten bokstav och skriver ut resultatet: "hej där, sweden!".

## Djupdykning:

Historiskt sett har skiftlägeskänslig strängbearbetning lett till många buggar och problem i program. Ruby undviker dessa problem med `.downcase` metoden, vilket gör jämförelser och bearbetning av strängdata mycket lättare. 

Alternativt kan du använda metoden `.downcase!` för att modifiera strängen direkt, istället för att skapa en ny kopia av strängen.

```Ruby
str = "HeJ DÄR, SwEdeN!"
str.downcase!
puts str
```

Detta gör samma sak som tidigare men ändrar den ursprungliga strängen direkt till: "hej där, sweden!" vilket kan vara mer minneeffektivt vid hantering av stora strängmängder.

## Se också:

Om du vill läsa mer om att arbeta med strängar i Ruby, kolla in följande länkar:

- Ruby's String Documentation: [https://ruby-doc.org/core-2.7.0/String.html](https://ruby-doc.org/core-2.7.0/String.html)
- Ruby's officiella guide för att arbeta med strängar: [https://guides.rubyonrails.org/action_view_overview.html#strings](https://guides.rubyonrails.org/action_view_overview.html#strings) 
- En bra artikel om olika sätt att manipulera strängar i Ruby: [https://www.rubyguides.com/2018/01/ruby-string-methods/](https://www.rubyguides.com/2018/01/ruby-string-methods/)