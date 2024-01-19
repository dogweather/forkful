---
title:                "Gör om en sträng till versaler"
html_title:           "Ruby: Gör om en sträng till versaler"
simple_title:         "Gör om en sträng till versaler"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att omvandla en sträng till stora bokstäver innebär att ändra alla bokstäver i en sträng till deras majuskelformer. Programmerare gör det för att underlätta jämförelser mellan strängar och förbättra läsbarheten.

## Hur man gör:
Ruby gör det enkelt genom att erbjuda den inbyggda metoden `upcase`. Så här använder du den:

```ruby
# Skapar en sträng
s = "hej världen!"

# Omvandlar strängen till stora bokstäver
puts s.upcase
```

Det här kommer att skriva ut:

```
HEJ VÄRLDEN!
```

## Djupdykning
Metoden `upcase` har varit en del av Ruby sedan de tidigaste versionerna av språket. Det är en del av standardbiblioteket för strängar, vilket innebär att du kan använda den direkt på alla strängobjekt du skapar.

Om du vill omvandla en sträng till stora bokstäver samtidigt som du bevarar några av dess ursprungliga bokstäver, t.ex. första bokstaven i varje ord, kan du använda `capitalize` eller` titleize` (det senare kräver aktiveringsdräkten).

Intern implementation av `upcase` i Ruby tar hänsyn till tecken uppslukning, vilket gör den mer tillförlitlig än att bara byta ASCII-värden.

## Se Även
Se [Ruby officiella dokumentationen](https://ruby-doc.org/core-2.5.1/String.html) för mer information om `upcase` och andra strängmetoder. Om du är intresserad av att förstå mer om strängars intern representation i Ruby, ta en titt på [denna artikel](https://www.honeybadger.io/blog/ruby-s-unicode-support/).