---
title:                "Sökning och ersättning av text"
html_title:           "Arduino: Sökning och ersättning av text"
simple_title:         "Sökning och ersättning av text"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Sökning och ersättning av text används för att hitta specifik information i en sträng och eventuellt ändra den. Detta görs ofta av programmerare för att automatisera processen att ändra kod.

## Så här gör du:

Du kan använda `gsub` funktionen i Ruby för att söka och ersätta text. 

```Ruby
str = "Hej Världen!"
ersatt_str = str.gsub('Världen', 'Ruby')
puts ersatt_str
```

Output:

```Ruby
Hej Ruby!
```

Här letar `gsub`-funktionen efter ordet "Världen" i strängen och ersätter det med "Ruby".

## Djupt Dyk:

Sökning och ersättning av text är en grundläggande operation som har funnits sedan de tidigaste programmeringsspråken skapades. I äldre språk som C var det dock ofta mycket mer komplicerat att genomföra.

I Ruby har vi `gsub` (global substitution) för sökning och ersättning, men det finns också `sub`, vilket bara gör en enskild ersättning.

Här är ett exempel:

```Ruby
str = "Hej Världen! Hej igen, Världen!"
ersatt_str = str.sub('Världen', 'Ruby')
puts ersatt_str
```

Output:

```Ruby
Hej Ruby! Hej igen, Världen!
```

Observera att `sub` bara ändrade den första instansen av "Världen", medan `gsub` skulle ha ändrat båda.

## Se också:

1. [Ruby Documentation: String#gsub](https://ruby-doc.org/core-2.7.0/String.html#method-i-gsub)
2. [Ruby Documentation: String#sub](https://ruby-doc.org/core-2.7.0/String.html#method-i-sub)
3. [How To Replace Text In A String In Ruby: An Overview Of The Sub And Gsub Method](https://www.rubyguides.com/2019/07/ruby-gsub-method/)