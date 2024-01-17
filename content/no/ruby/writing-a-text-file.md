---
title:                "Å skrive en tekstfil"
html_title:           "Ruby: Å skrive en tekstfil"
simple_title:         "Å skrive en tekstfil"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Hva er og hvorfor?

Skriver du ofte programmer? Da vil du sikkert også trenge å skrive tekstdokumenter som kan leses og brukes av andre programmer. Dette kalles å skrive en tekstfil, og det er en måte å lagre og strukturere data på en effektiv og lesbar måte. Det er en viktig del av å være en programmerer, fordi det lar deg dele og utveksle informasjon mellom forskjellige programmer.

## Slik gjør du det:

```Ruby
# Åpne en tekstfil for å skrive til den
file = File.new("tekstfil.txt", "w")

# Skriv innhold til filen
file.puts "Dette er en tekstfil"
file.puts "Du kan skrive flere linjer også"

# Lukk filen
file.close
```

Når du kjører dette programmet, vil det opprette en ny tekstfil kalt "tekstfil.txt" og skrive innholdet du har angitt i koden. For å lese innholdet fra filen, kan du bruke følgende kode:

```Ruby
# Åpne en tekstfil for å lese fra den
file = File.new("tekstfil.txt", "r")

# Skriv ut hver linje fra filen
file.each do |line|
  puts line
end

# Lukk filen
file.close
```

Output vil være:

```
Dette er en tekstfil
Du kan skrive flere linjer også
```

## Dykk dypere:

Å kunne skrive til tekstfiler har vært en viktig del av programmering siden tidlig i bransjens historie. Før datamaskiner hadde grafiske grensesnitt, var tekstfiler den eneste måten å lagre og strukturere data på. Alternativene til å skrive en tekstfil inkluderer å bruke en database eller et regnearkprogram, men tekstfiler er ofte mer fleksible og kan enkelt leses og brukes av alle programmeringsspråk.

For mer informasjon om hvordan du bruker tekstfiler i Ruby, kan du se Ruby sin dokumentasjon på Filklassen: https://ruby-doc.org/core-2.7.0/File.html

## Se også:

- [Den offisielle Ruby-dokumentasjonen](https://ruby-doc.org/)
- [En veiledning til å arbeide med tekstfiler i Ruby](https://www.rubyguides.com/2015/05/working-with-files-ruby/)