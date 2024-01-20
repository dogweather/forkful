---
title:                "Kontrollera om en katalog finns"
date:                  2024-01-20T14:58:58.315979-07:00
html_title:           "Fish Shell: Kontrollera om en katalog finns"
simple_title:         "Kontrollera om en katalog finns"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att kontrollera om en katalog existerar är att se till att en specifik mappväg finns på systemet. Programmerare gör detta för att undvika fel som beror på att man försöker läsa från eller skriva till icke-existerande kataloger.

## Hur gör man:
Använd `Dir.exist?` för att snabbt kolla om en katalog finns.

```Ruby
if Dir.exist?("önskad/katalog")
  puts "Katalogen finns!"
else
  puts "Katalogen finns inte."
end
```

Resultatet blir antingen "Katalogen finns!" eller "Katalogen finns inte." beroende på om katalogen existerar.

## Djupdykning:
Funktionen `Dir.exist?` introducerades i Ruby 1.9 som en del av `Dir`-klassen och ersatte den äldre `File.exists?` som också kunde användas för att kontrollera kataloger. Ett alternativ är `File.directory?`, som likaså kan användas för att kontrollera om en katalog existerar och är just en katalog, inte en fil.

Historiskt sett har felhantering kring filsystemet alltid varit centralt inom programmering. Att skapa, läsa, och skriva till filer eller kataloger utan att först kontrollera om de existerar kan leda till krascher eller dataförlust.

När man implementerar kontroll av kataloger är det smart att också hantera potentiella säkerhetsrisker. Användaren kan till exempel försöka åtkomma kataloger utanför programmets räckvidd eller rättigheter.

## Se även:
- Ruby-dokumentation för `Dir`-klassen: [https://ruby-doc.org/core/Dir.html](https://ruby-doc.org/core/Dir.html)
- Ruby-dokumentation för `File`-klassen: [https://ruby-doc.org/core/File.html](https://ruby-doc.org/core/File.html)
- Stack Overflow-diskussioner om när och varför kontrollera katalogers existens: [https://stackoverflow.com/questions/tagged/ruby+directory](https://stackoverflow.com/questions/tagged/ruby+directory)