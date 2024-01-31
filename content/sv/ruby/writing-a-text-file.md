---
title:                "Skriva en textfil"
date:                  2024-01-19
html_title:           "Arduino: Skriva en textfil"
simple_title:         "Skriva en textfil"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Vad och Varför?
Skriva en textfil innebär att lagra data i en läsbar form på din hårddisk. Programmerare gör detta för att spara konfigurationer, loggar eller annan utdata som kan användas senare eller av andra program.

## Hur gör man:
```Ruby
# Skapar och skriver till en ny fil
File.open('exempel.txt', 'w') do |file|
  file.puts "Hej, det här är en text i en fil!"
end

# Lägger till text i en befintlig fil
File.open('exempel.txt', 'a') do |file|
  file.puts "Här kommer lite mer text."
end

# Läs den skrivna filen
puts File.read('exempel.txt')
```
Sample output:
```
Hej, det här är en text i en fil!
Här kommer lite mer text.
```

## Fördjupad information:
Förr när diskutrymmet var mer begränsat och dyrt, använde programmerare textfiler för enkel lagring och konfigivering. Alternativ till textfiler inkluderar databaser och andra dataformat som JSON och XML. Detaljerna i att skriva till en fil i Ruby är hanterade av IO klassen, där File är en subklass. Det handlar till stor del om att öppna en kanal till filen, skicka data till den, och sedan stänga den.

## Se även:
- [Ruby File Class](https://ruby-doc.org/core-2.7.0/File.html)
