---
title:                "Ruby: Jämföra två datum"
programming_language: "Ruby"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Varför

Att jämföra två datum är en viktig del av programmering, särskilt för att säkerställa att programmet hanterar korrekt data och att det fungerar som det ska. Det är också ett vanligt problem som många utvecklare stöter på i sitt dagliga arbete. I denna bloggpost kommer vi att gräva djupare in i hur man jämför två datum med hjälp av Ruby.

## Så här gör du

För att jämföra två datum i Ruby måste vi först skapa två instanser av Date-klassen. Detta kan göras på olika sätt beroende på hur dina datum är sparade. Om de är sparade som strängar, kan vi använda metoden `Date.parse` för att konvertera dem till instanser av Date-klassen. Om de redan är instanser av Date-klassen behöver vi bara använda dem som de är.

```Ruby
date1 = Date.parse("2021-01-01")
date2 = Date.new(2021, 1, 1)
```

När vi har våra två datum kan vi använda olika metoder för att jämföra dem. En av de enklaste är metoden `==` som returnerar `true` om de två datum är exakt lika.

```Ruby
date1 == date2  # true
```

Vi kan också använda metoden `>` eller `<` för att se om ett datum är större eller mindre än det andra.

```Ruby
date1 > date2  # false
```

Om vi vill jämföra specifika aspekter av datumen, som till exempel år, månad eller dag, kan vi använda metoder som `year`, `month` eller `day`.

```Ruby
date1.year > date2.year  # false
```

## Djupdykning

När vi jämför två datum i Ruby är det viktigt att ha en förståelse för hur datumen sparas och hanteras. Datumen är sparade som instanser av Date-klassen, som i sin tur är baserad på en juliansk kalender. Detta kan leda till oväntade resultat när vi jämför datumen, särskilt när det gäller skottår.

En annan viktig aspekt att tänka på är att datumen också innehåller information om tid, vilket kan påverka jämförelsen om det är olika mellan de två datumen. Det är därför viktigt att kontrollera tiden också om vi vill göra en mer exakt jämförelse.

## Se även

- [Ruby's Date-klass dokumentation](https://ruby-doc.org/stdlib-3.0.1/libdoc/date/rdoc/Date.html)
- [Skillnaden mellan två datumn i Ruby](https://blog.appsignal.com/2019/05/21/finding-the-difference-between-two-dates-in-ruby.html)
- [Jämföra datum och tid i Ruby](https://blog.appsignal.com/2021/04/28/comparing-dates-and-times-in-ruby.html)