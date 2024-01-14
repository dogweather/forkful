---
title:                "Ruby: Sammanfogning av strängar"
simple_title:         "Sammanfogning av strängar"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/concatenating-strings.md"
---

{{< edit_this_page >}}

## Varför

Att sammanslå strängar eller *concatenation* som det också kallas är en vanlig operation inom programmering. Genom att kombinera flera strängar till en enda kan man skapa en mer komplex text som kan användas för olika ändamål. Det kan till exempel vara användbart när man vill skapa en dynamisk text för användaren baserat på olika variabler.

## Hur man gör

För att sammanslå strängar i Ruby använder man operatorn `+` eller metoden `concat`. Nedan följer några exempel på hur man kan använda dessa i sin kod:

```Ruby
str1 = "Hej"
str2 = "världen!"

puts str1 + str2
# Output: Hej världen!

puts str1.concat(str2)
# Output: Hej världen!
```

Som du ser blir resultatet samma oavsett om vi använder `+` eller `concat`. Det är en smaksak vilken man väljer, men det är bra att vara medveten om att `+` kan användas för att även kombinera andra datatyper som siffror och boolean-värden, medan `concat` endast fungerar på strängar.

Om du vill använda sammanslagna strängar som värden för variabler kan du göra på följande sätt:

```Ruby
str1 = "Det är "
str2 = "viktigt att vara "
str3 = "inkluderande."

ny_sträng = str1 + str2 + str3

puts ny_sträng
# Output: Det är viktigt att vara inkluderande.
```

Som du ser går det också att kombinera flera strängar i en enda rad genom att använda `+` flera gånger.

## Deep Dive

När man sammanslår strängar måste man vara uppmärksam på vilket tecken man använder mellan dem, eftersom det kan påverka resultatet. En vanlig fallgrop är att glömma mellanslag mellan orden. Om du till exempel sammanslår strängarna `"Hej"` och `"världen!"` utan mellanslag kommer resultatet att bli `"Hejvärlden!"`, vilket kanske inte var det du var ute efter.

Även specialtecken som `\n` (radbrytning) och `\t` (tabb) kan sättas in mellan strängar för att formatera texten på ett önskat sätt.

## Se även

Här är några ytterligare resurser som kan vara användbara för att lära sig mer om att sammanslå strängar i Ruby:

- [Official Ruby Documentation](https://ruby-lang.org/en/documentation/)
- [Ruby Guides - Strings](https://www.rubyguides.com/2015/06/ruby-strings/)
- [Ruby String Concatenation](https://www.geeksforgeeks.org/ruby-string-concatenation/)