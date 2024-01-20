---
title:                "Ta bort tecken som matchar ett mönster"
html_title:           "Arduino: Ta bort tecken som matchar ett mönster"
simple_title:         "Ta bort tecken som matchar ett mönster"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Vad & Varför? 
Att radera tecken som matchar ett mönster innebär att få bort specifika karaktärer från en sträng i Ruby. Programmerare gör det för att manipulera data för specifika behov; kanske för att rensa oönskade tecken eller hantera formatering.

## Hur man gör:
Här är hur du kan radera tecken med Ruby's inbyggda 'delete' metoden:

```Ruby
str = "Hej, Världen!"
ny_str = str.delete('!')
puts ny_str
# Output: "Hej, Världen"
```

'Match' ger oss möjlighet att använda reguljära uttryck för mer avancerade mönster:

```Ruby
str = "Varför, varför, varför?"
ny_str = str.gsub(/varför/i, '')
puts ny_str
# Output: ", , "
```

## Djupdykning
Historiskt sett har behovet av att manipulera strängar varit en central del av datahantering. Det är därför Ruby, som skapades på mitten av 90-talet, redan hade metoder för hantering av strängmanipulation.

Du kan också använda 'tr' metoden som ett alternativ till 'delete':

```Ruby
str = "Hej, Världen!"
ny_str = str.tr('!', '')
puts ny_str
# Output: "Hej, Världen"
```

Huruvida du ska använda 'delete', 'gsub', eller 'tr' beror mycket på vilken typ av problemlösning du står inför och personlig preferens.

## Se Även
För mer läsning, kolla in dessa källor:
- [Ruby's Officiella Dokumentation om Strängar](https://ruby-doc.org/core/String.html)
- [TutorialsPoint's Ruby Tutorial](https://www.tutorialspoint.com/ruby/index.htm)