---
title:                "Ruby: Skrive en tekstfil"
simple_title:         "Skrive en tekstfil"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/writing-a-text-file.md"
---

{{< edit_this_page >}}

#Hvorfor

Det å skrive en tekstfil er en vanlig oppgave for Ruby-programmerere. Ved å lære denne grunnleggende ferdigheten, kan du enkelt lagre informasjon i en fil som kan behandles og leses av datamaskinen din.

#Slik gjør du det

Å skrive en tekstfil i Ruby er enkelt, takket være de innebygde funksjonene som er tilgjengelige. Se den følgende koden for et eksempel:

```Ruby
# Åpne en fil for skriving
min_fil = File.open("min_fil.txt", "w")

# Skriv til filen
min_fil.write("Hei, dette er en tekstfil som skrives i Ruby!")

# Lukk filen
min_fil.close()
```

Etter å ha kjørt dette kodesnippet, vil du merke at en ny fil, "min_fil.txt", er laget i samme mappe som Ruby-filen din. Denne filen vil inneholde teksten du skrev inn i koden.

#Dypdykk

Nå som du har lært det grunnleggende, kan du utforske mer avanserte funksjoner for å skrive tekstfiler i Ruby. For eksempel kan du lese innholdet i en eksisterende fil ved å bruke "File.read" -funksjonen, og du kan også kombinere tekst og variabler med "File.puts" -funksjonen.

En annen nyttig funksjon er "File.open" i kombinasjon med blokker. Blokker lar deg utføre en serie med instruksjoner innenfor en bestemt omfang. I dette tilfellet vil en blokk sikre at filen automatisk blir lukket uansett om koden din krasjer. Her er et eksempel:

```Ruby
# Åpne en fil ved hjelp av en blokk
File.open("min_fil.txt", "w") do |f|
  # Skriv til filen
  f.write("Dette er en blokk for å skrive tekstfiler i Ruby!")
end
```

#Se også

For mer informasjon om å skrive tekstfiler i Ruby, sjekk ut følgende ressurser: 

- [RubyDocs: File Class](https://ruby-doc.org/core-3.0.1/File.html)
- [RubyGuides: How to Write to a File in Ruby](https://www.rubyguides.com/ruby-tutorial/io/write-to-file/)
- [Codecademy: Learn Ruby](https://www.codecademy.com/learn/learn-ruby)