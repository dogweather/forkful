---
title:                "Omvandla ett datum till en sträng"
html_title:           "C#: Omvandla ett datum till en sträng"
simple_title:         "Omvandla ett datum till en sträng"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att konvertera ett datum till en sträng är när man tar ett datumobjekt och omvandlar det till en läsbar textsträng. Programmerare gör detta för att förenkla visningen eller lagringen av datum i programvaruapplikationer.

## Hur gör man:
 
För att konvertera ett datum till en sträng i Ruby, använd metoden `to_s`.
 
```Ruby 
datum = Date.today
datum_sträng = datum.to_s
puts datum_sträng
```

Detta kommer att skriva ut datumet som en sträng i formatet "YYYY-MM-DD".

## Djupdykning:

Att ändra ett datum till en sträng är ett viktigt verktyg för alla Ruby-programmerare. Denna teknik har funnits sedan Ruby skapades på 90-talet.

En alternativ metod i Ruby är att använda `strftime`-metoden, vilket är kraftfullt eftersom det tillåter specifika format att anges.

```Ruby
puts Date.today.strftime("%d-%m-%Y")
```

Detta kommer att skriva ut datumet som en sträng i formatet "DD-MM-YYYY".

Koden bakom metoden är relativt enkel. Det är bara att ta datumvärdena och skriva ut dem som en textsträng.

## Se även:

För mer information om datumhantering i Ruby, kolla:
- Ruby officiella dokumentation: [Date](https://ruby-doc.org/stdlib-2.5.1/libdoc/date/rdoc/Date.html)
- [Formatting DateTime](https://www.rubyguides.com/2015/12/ruby-time/)
- StackOverflow trådar om datumhantering i Ruby: [StackOverflow](https://stackoverflow.com/questions/tagged/ruby+date)