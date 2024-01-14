---
title:    "Ruby: Konvertering av en datum till en sträng"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

##Varför

Att konvertera ett datum till en sträng är en vanlig uppgift för många Ruby-programmerare. Detta gör det möjligt att visa datum i ett läsbart och förståeligt format för användare eller för att spara data till en databas eller fil.

##Så här

För att konvertera ett datum till en sträng kan du använda `strftime`-metoden. Den här metoden tar ett datumobjekt och en sträng med specifierare och returnerar en sträng med det önskade formatet.

```Ruby
date = Date.today
date.strftime("%d-%m-%Y") # "01-09-2020"
```

Här är några vanliga specifierare som kan användas för att konfigurera det genererade datumet:

- %d: Dag i månaden (1 till 31)
- %m: Månad (1 till 12)
- %Y: År med fyra siffror
- %y: År med två siffror
- %b: Kort månadsnamn (t.ex. "Jan")
- %B: Fullständigt månadsnamn (t.ex. "Januari")
- %a: Kort veckodagsnamn (t.ex. "Mån")
- %A: Fullständigt veckodagsnamn (t.ex. "Måndag")

Du kan också kombinera specifierarna för att skapa ett anpassat format. Till exempel, om du vill ha ett datum i formatet "YYYY-MM-DD", kan du använda `%Y-%m-%d`.

Om du vill lägga till tider i din sträng kan du använda `%H` (timme), `%M` (minut) och `%S` (sekund) specifierare. Till exempel:

```Ruby
date = Time.now
date.strftime("%H:%M:%S") # "15:43:21"
```

##Djupdykning

Bakom kulisserna använder Ruby-biblioteket `strftime`-metoden `strptime` för att konvertera datumet till en sträng. Den här metoden är förmågan att extrahera tider och datum från en sträng med hjälp av angivna specifierare. Detta kan vara särskilt användbart när du behöver konvertera en användarens inmatade sträng till ett datum.

För att se en fullständig lista över specifierare och deras betydelser kan du kolla på Ruby-dokumentationen för Time-klassen.

##Se även

- [Date and Time i Ruby](https://ruby-doc.org/stdlib-2.7.1/libdoc/date/rdoc/DateTime.html)
- [Time-klassen dokumentation](https://ruby-doc.org/core-2.7.1/Time.html)
- [Ruby strftime formatter](https://www.garethrees.co.uk/2013/05/04/time-strftime)
- [Ruby Date & Time Programming Guide](https://www.rubyguides.com/2015/05/ruby-time/)