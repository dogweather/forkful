---
title:                "Analysera ett datum från en sträng"
html_title:           "Kotlin: Analysera ett datum från en sträng"
simple_title:         "Analysera ett datum från en sträng"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att tolka ett datum från en sträng innebär att omvandla en textrepresentation av ett datum till ett egentligt datum-objekt. Det gör programmerare för att hantera datuminformation på ett enhetligt och lättanvänt sätt.

## Hur:

Nu ska vi använda Ruby`s inbyggda bibliotek `Date` för att demonstrera:

```Ruby
require 'date'

str = '2022-05-01'
datum = Date.parse(str)

puts datum
```

Utgivenheten skulle vara:

```Ruby
2022-05-01
```

Det är så enkelt att göra det i Ruby. Standardbiblioteket `Date` tar varje sträng i "ÅÅÅÅ-MM-DD" format och omvandlar den till ett datumobjekt.

## Djupdykning:

1. **Historiskt sammanhang:** Tolkning av datum från strängar har varit nödvändigt sedan programmerares tidiga dagar. När människor började lagra datum i databaser, uppstod behovet att kunna omvandla dem till något mer användbart i kod.

2. **Alternativ:** Du kan också använda `strptime` -metoden om ditt datum inte följer standardformatet. T.ex:

    ```Ruby
    str = '01-May-2023'
    datum = Date.strptime(str, '%d-%b-%Y')
    
    puts datum
    ```

    Detta ger oss tillbaka:

    ```Ruby
    2023-05-01
    ```

3. **Implementeringsdetaljer:** Metoderna `parse` och `strptime` tillhör Ruby`s inbyggda `Date` -klass, vilket gör dem lätta att använda direkt ur boxen.

## Se Också:

1. [Ruby Date Documentation](https://ruby-doc.org/stdlib-2.5.1/libdoc/date/rdoc/Date.html)