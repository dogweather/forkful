---
title:    "Ruby: Hitta längden på en sträng"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Varför
Att hitta längden på en sträng är en vanlig uppgift vid programmering. Det kan vara användbart för att kontrollera inmatningsdata eller för att hantera textsträngar i ditt program. Att kunna hitta längden på en sträng är en grundläggande kunskap som kommer att vara till nytta oavsett vad du programmerar i Ruby.

## Hur man gör det
För att hitta längden på en sträng i Ruby, använder du metoden `length` eller `size`. Båda dessa metoder fungerar på samma sätt och returnerar längden på strängen som en numerisk värde. Se följande exempel:

```Ruby
text = "Hej världen"
puts text.length  # output: 11
puts text.size  # output: 11
```

Som du kan se i exemplet ovan, är "Hej världen" en 11-tecken lång sträng. Genom att använda metoden `length` eller `size` kan du hitta längden på en sträng på ett enkelt sätt i ditt Ruby-program.

## Djupdykning
Du kanske undrar vilken metod, `length` eller `size`, som är bättre att använda. Svaret är att det inte finns någon skillnad mellan dem när det gäller att hitta längden på en sträng. De kan båda utföra samma uppgift på samma sätt. Valet mellan `length` och `size` beror på personliga preferenser och kanske vana.

Det är dock viktigt att notera att både `length` och `size` är metoder som endast fungerar på strängar. Om du försöker använda dem på andra datatyper, såsom en numeriskt värde eller en array, kommer de inte att returnera ett resultat.

## Se också
- [Ruby Dokumentation om Strings](https://ruby-doc.org/core-2.6.3/String.html)
- [Ruby Metoder för Strings](https://www.rubyguides.com/ruby-string-methods/)
- [En enkel guide till att programmera i Ruby](https://www.digitalocean.com/community/tutorials/how-to-program-in-ruby)