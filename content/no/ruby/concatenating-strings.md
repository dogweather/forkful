---
title:    "Ruby: Sammenslåing av strenger"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/ruby/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hvorfor

Å kombinere, eller konkatenere, strenger er en nødvendig og vanlig oppgave i Ruby-programmering. Ved å samle sammen separate strenger kan du lage mer komplekse uttrykk og variabler. Dette gjør koden din mer dynamisk og funksjonell.

## Hvordan

For å konkatenere strenger i Ruby, kan du bruke "+" operatøren. La oss si vi har to forskjellige navn som vi vil kombinere til en ny streng:

```Ruby
navn1 = "Hans"
navn2 = "Grete"
```

Vi kan enkelt kombinere disse to strengene ved hjelp av "+":

```Ruby
navn3 = navn1 + navn2
puts navn3

# Output: HansGrete
```

Som du ser, blir strengene direkte satt sammen uten mellomrom eller komma. Men du kan også legge til disse ekstra tegnene hvis du vil ha det med:

```Ruby
navn3 = navn1 + " " + navn2
puts navn3

# Output: Hans Grete
```

Du kan også bruke "<<" operatøren for å legge til en streng til en eksisterende variabel. Dette gjør at koden din blir mer effektiv og lesbar:

```Ruby
fullt_navn = "Hans"
fullt_navn << " "
fullt_navn << "Grete"
puts fullt_navn

# Output: Hans Grete
```

## Dypdykk

I Ruby kan du også bruke .concat metoden for å konkatenere strenger. Denne metoden legger til strenger til en eksisterende variabel uten å lage en ny:

```Ruby
navn = "Hans"
navn.concat " Grete"
puts navn

# Output: Hans Grete
```

En annen nyttig metode for å konkatenere strenger er .strip, som lar deg fjerne alle white spaces rundt strenger. Dette er spesielt nyttig når du jobber med brukerinndata eller hvis du vil ha en mer ryddig utskrift:

```Ruby
navn1 = " Hans "
navn2 = "Grete"
fullt_navn = navn1.strip.concat(navn2)
puts fullt_navn

# Output: HansGrete
```

## Se også

- [Ruby String documentation](https://ruby-doc.org/core-2.6/String.html)
- [Ruby Tips - Concatenating Strings](https://blog.appsignal.com/2018/07/31/ruby-magic-concatenating-strings.html)