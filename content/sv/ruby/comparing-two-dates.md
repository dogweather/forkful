---
title:    "Ruby: Jämföra två datum"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Varför

Att jämföra två datum är en viktig del av Ruby-programmering eftersom det kan hjälpa oss att hantera tid och datum effektivt. Det är också en viktig grundläggande färdighet som behövs för att bygga mer komplexa program.

## Så gör du

För att jämföra två datum i Ruby, används operatorerna `<`, `>`, `==` och `===` beroende på vad du vill jämföra. Om du vill se om ett datum är före eller efter ett annat använder du `<` och `>`. Om du vill se om de är exakt samma datum använder du `==` eller `===`.

```Ruby
date1 = Date.new(2021, 9, 1)
date2 = Date.new(2021, 10, 1)

puts date1 < date2 #Output: true
puts date1 > date2 #Output: false
puts date1 == date2 #Output: false
puts date1 === date2 #Output: false
```

Det finns också en speciell metod som heter `between?` som kan användas för att kontrollera om ett datum ligger mellan två andra datum.

```Ruby
date3 = Date.new(2021, 9, 15)

puts date3.between?(date1, date2) #Output: true
```

## Djupdykning

När det gäller att jämföra datum i Ruby finns det några saker att tänka på. För det första är det viktigt att veta vilka typer av objekt som faktiskt kan jämföras. Dessa inkluderar `Date`, `Time`, `DateTime` och `Numeric` objekt.

För det andra är det viktigt att hålla koll på vilken tidszon du arbetar med. Om du har datum och tider i olika tidszoner kan det påverka resultaten av dina jämförelser.

Det är också värt att notera att jämförelser av datum och tider i Ruby inte är exakta på nanosekundnivå. Om du behöver jämföra väldigt exakta datum och tider kan det vara bättre att använda `DateTime` objekt som kan hantera mer exakta tidsintervall.

## Se även

Här är några användbara länkar för att lära dig mer om jämförelse av datum i Ruby:

- [Ruby Date klass dokumentation](https://ruby-doc.org/stdlib-3.0.2/libdoc/date/rdoc/Date.html)
- [Ruby Time klass dokumentation](https://ruby-doc.org/stdlib-3.0.2/libdoc/time/rdoc/Time.html)
- [Ruby DateTime klass dokumentation](https://ruby-doc.org/stdlib-3.0.2/libdoc/datetime/rdoc/DateTime.html)
- [Artikel om jämförelser i Ruby](https://www.rubyguides.com/2016/08/ruby-comparable-module/)

Tack för att du läste! Kom ihåg att jämförelse av datum är en viktig färdighet att behärska när du arbetar med tid och datum i dina Ruby-program. Lycka till!