---
title:                "Ruby: Skapa en tillfällig fil"
simple_title:         "Skapa en tillfällig fil"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Varför 

I många tillfällen när vi utvecklar program måste vi tillfälligt skapa en fil för att lagra data eller utföra operationer som behöver ett fysiskt utrymme. Att skapa en temporär fil är ett vanligt sätt att hantera detta problem, och det är en enkel och effektiv lösning som gör att våra program kan fungera smidigt. 

## Hur 

För att skapa en temporär fil i Ruby kan vi använda metoden `Tempfile.create` och ange en önskad filändelse. Här är en kod som visar hur detta kan göras: 

```Ruby 
# Skapa en temporär fil med filändelse .txt 
tempfil = Tempfile.create(['ruby', '.txt']) 
``` 

Vi kan även använda en blockstruktur för att se till att filen stängs och raderas automatiskt när vi är klara med den: 

```Ruby 
Tempfile.create(['ruby', '.txt']) do |tempfil| 
  # Koda här för att göra något med den temporära filen 
end 
``` 

När den temporära filen skapas kommer den att ha ett unikt namn, vilket vi kan få tillgång till genom att anropa `tempfil.path`. Vi kan även skriva till filen genom att använda `tempfil.write` och läsa från filen genom att använda `tempfil.read`. När vi är klara med filen kan vi stänga den genom att anropa `tempfil.close`. 

Om vi vill ha mer kontroll över var den temporära filen ska skapas och hur den ska namnges kan vi ange dessa inställningar som argument till metoden. Här är ett exempel på hur vi kan göra detta: 

```Ruby 
# Skapa en temporär fil i mappen "tmp" och ange "ruby" som prefix 
Tempfile.create(['ruby', '.txt'], 'tmp') do |tempfil|
  # Gör något med filen här 
end 
``` 

## Djupdykning 

När vi använder `Tempfile.create` i Ruby skapas faktiskt en instans av klassen `Tempfile`, som finns som en del av Ruby's standardbibliotek. Denna klass innehåller flera metoder för att hantera den temporära filen, såsom att ändra filändelsen, flytta filen eller skriva till den i binärt läge. 

En intressant egenskap hos `Tempfile` är att den kommer att raderas automatiskt när den skapas, om vi inte tar bort detta beteende. Detta kan vara användbart om vi bara vill ha den temporära filen under en kort tid och sedan ta bort den igen. För att ändra detta beteende kan vi använda `Tempfile.open` istället för `Tempfile.create`. 

## Se även 

- Tempfile dokumentation: https://ruby-doc.org/stdlib-3.0.1/libdoc/tempfile/rdoc/Tempfile.html 
- Mer om Ruby standardbiblioteket: https://ruby-doc.org/stdlib-3.0.1/