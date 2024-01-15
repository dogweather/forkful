---
title:                "Att skriva en textfil"
html_title:           "Elixir: Att skriva en textfil"
simple_title:         "Att skriva en textfil"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/writing-a-text-file.md"
---

{{< edit_this_page >}}

##Varför

Att skriva en textfil är en viktig del av programmering eftersom det låter oss spara information och använda den senare. Det är också ett enkelt sätt att läsa och dela data mellan olika enheter och program.

##Hur man gör det

För att skriva en textfil i Elixir, behöver vi först öppna en ny fil med hjälp av funktionen ```File.open()```. Sedan kan vi använda ```IO.write()``` för att skriva vår text in i filen och avsluta med att stänga filen med ```File.close()```. Här är ett exempel på kod och utmatning:

````elixir
# Skapa en ny fil med namnet "hello.txt" och öppna den för skrivning
{:ok, file} = File.open("hello.txt", [:write])

# Skriv in texten "Hej världen!" i filen
IO.write(file, "Hej världen!")

# Stäng filen
:ok = File.close(file)

# Kontrollera att filen har skapats
[hello.txt] skapad och "Hej världen!" skrivet in i den.
````

##Djupdykning

I Elixir finns det flera alternativ för att skriva in data i en textfil. Till exempel kan vi använda ```IO.puts()``` istället för ```IO.write()``` för att automatiskt lägga till en radbrytning efter varje utmatning. Vi kan också använda ```IO.binwrite()``` för att läsa binära data. Dessutom kan vi skapa filer på specifika platser eller ändra filens rättigheter.

##Se även

- [Elixir IO-dokument](https://hexdocs.pm/elixir/IO.html)
- [Filmodul i Elixir](https://hexdocs.pm/elixir/File.html)
- [Elixir Markdown](https://hexdocs.pm/elixir/Markdown.html)