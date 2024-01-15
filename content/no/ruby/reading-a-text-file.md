---
title:                "Lese en tekstfil"
html_title:           "Ruby: Lese en tekstfil"
simple_title:         "Lese en tekstfil"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/reading-a-text-file.md"
---

{{< edit_this_page >}}

##Hvorfor
Hvorfor ville noen lese en tekstfil i Ruby? Vel, hvis du vil håndtere ulike data i programmet ditt, kan det være nødvendig å lese informasjonen fra en tekstfil. Dette kan være nyttig for å organisere og behandle store mengder data på en strukturert måte.

##Slik gjør du det
For å lese en tekstfil i Ruby, må du først åpne filen. Dette gjøres ved hjelp av `File.open` metoden, som tar inn banen til filen som et argument. Deretter kan du bruke `readlines` metoden for å lese innholdet i filen linje for linje.

For eksempel, hvis vi har en fil kalt "numbers.txt" med følgende innhold:
```
1
2
3
4
5
```
Da kan vi lese filen og skrive ut linjene ved å bruke følgende kode:
```Ruby
file = File.open("numbers.txt") # Åpner filen
file.readlines.each do |line| # Bruker readlines metoden for å lese innholdet
  puts line # Skriver ut linjene
end
```

Dette vil resultere i følgende utskrift:
```
1
2
3
4
5
```

##Dypdykk
I tillegg til å kunne lese en tekstfil linje for linje, kan du også få tilgang til forskjellige deler av linjen ved å bruke metoden `split`. Dette deler linjen inn i en array basert på et angitt tegn. For eksempel, hvis vi vil få tak i tallene i filen vår og regne ut summen av dem, kan vi bruke følgende kode:
```Ruby
file = File.open("numbers.txt")
numbers = file.readlines # Lagrer filens innhold i en variabel
sum = 0 # Initialiserer sum variabelen
numbers.each do |line| # Går gjennom hver linje i filen
  sum += line.split(',').map(&:to_i).sum # Splitter linjen ved komma og legger til tallene i sum variabelen
end

puts sum # Skriver ut summen
```

Dette vil resultere i utskriften:
```
15
```
Et annet nyttig tips når du leser tekstfiler i Ruby er å bruke `File.foreach` metoden i stedet for `File.open` og `readlines` metoden. Dette betyr at Ruby vil lese filen en linje om gangen, noe som er mer effektivt for store filer.

##Se også
For mer informasjon om å lese og håndtere tekstfiler i Ruby, kan du sjekke ut følgende ressurser:

- [Ruby Documentation - File](https://ruby-doc.org/core-3.0.1/File.html)
- [The Pros and Cons of Different Ways of Reading and Writing Files in Ruby](https://rorguide.blogspot.com/2014/05/the-pros-and-cons-of-different-ways-of.html)
- [Reading and Writing Files in Ruby – Part 1](https://www.rubyguides.com/2015/05/working-with-files-ruby/#Reading_Files_Line_by_Line)