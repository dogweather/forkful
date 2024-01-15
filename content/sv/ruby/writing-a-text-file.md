---
title:                "Skriva en textfil"
html_title:           "Ruby: Skriva en textfil"
simple_title:         "Skriva en textfil"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Varför

Du kanske undrar varför du skulle vilja skriva en textfil i Ruby när det finns andra enklare sätt att spara information. Men ibland kan det vara användbart att kunna skapa en textfil som kan läsas och redigeras av både människor och datorer.

## Så här gör du

För att skapa en textfil i Ruby kan du använda dig av File-klassen. Här är ett exempel på hur du kan skapa en ny textfil och skriva in lite information i den:

```Ruby
file = File.open("mitt_exempel.txt", "w")  #Öppnar en ny textfil för skrivning
file.puts "Det här är en textfil skriven i Ruby!"  #Skriver in en sträng i textfilen
file.puts "Hoppas du har det bra idag!"  
file.close  #Stänger filen
```

Om du nu öppnar filen du skapade så borde du se följande innehåll:

```
Det här är en textfil skriven i Ruby!
Hoppas du har det bra idag!
```

Du kan också lägga till information i en befintlig textfil genom att öppna den i läget "a" istället för "w":

```Ruby
file = File.open("mitt_exempel.txt", "a")  #Öppnar textfilen för append (lägg till)
file.puts "Här är mer text som läggs till i slutet av filen!"
file.close
```

## Djupdykning

När du använder File-klassen för att skapa en textfil så skapas den automatiskt i den aktuella mappen där ditt Ruby-program körs. Om du vill skapa filen på en annan plats kan du ange hela sökvägen till filen istället, t.ex. "Users/min_dator/mitt_exempel.txt".

En annan viktig aspekt när det gäller att skriva textfiler är att se till att filen stängs efter att du är klar med den. Om du glömmer att stänga filen så kan det leda till oönskade buggar och problem i ditt program. Genom att använda metoden "close" för filen så ser du till att all information sparas och att filen inte längre är öppen för redigering.

## Se även

- [Ruby File-klass dokumentation](https://ruby-doc.org/core-2.7.1/File.html)
- [En bloggartikel om textfilshantering i Ruby](https://www.joshwcomeau.com/ruby/file-handling/)
- [En guide till Ruby på svenska](https://rubytutorial.se/)