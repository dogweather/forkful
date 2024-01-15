---
title:                "Kontrollera om en mapp finns"
html_title:           "Ruby: Kontrollera om en mapp finns"
simple_title:         "Kontrollera om en mapp finns"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Varför
Det är viktigt att kunna kontrollera om en mapp finns på en dator när man skriver kod i Ruby. Detta kan hjälpa till att undvika felmeddelanden och felaktig kod som kan orsaka störningar.

## Hur man gör
För att kontrollera om en mapp finns i Ruby så kan man använda sig av en metod som heter `Dir.exist?()`. Denna metod tar en sträng som argument, vilken ska vara sökvägen till den mapp som man vill kontrollera. Om mappen finns kommer metoden att returnera `true` och om den inte finns så returnerar den `false`.

```Ruby
# Kontrollera om mappen "documents" finns
if Dir.exist?("documents")
  puts "Mappen finns"
else
  puts "Mappen finns inte"
end
```

I det här exemplet så används en `if/else` sats för att skriva ut ett meddelande beroende på om mappen finns eller inte. Om man endast vill göra en handling om mappen finns så kan man använda `Dir.exist?()` i en `if` sats utan `else`.

```Ruby
# Skriv ut en lista på filerna i mappen "downloads" om den finns
if Dir.exist?("downloads")
  # Lista alla filer i mappen
  Dir.each_child("downloads") { |file| puts file }
end
```

### Deep Dive
Om man vill kontrollera en mapp på en annan plats än där ens Ruby-kod körs, så kan man använda `Dir.exist?()` tillsammans med `File.expand_path()` för att få en absolut sökväg till mappen. Detta kan vara användbart när man arbetar med olika datorer eller filsystem.

```Ruby
# Kontrollera om mappen "downloads" finns på skrivbordet
if Dir.exist?(File.expand_path("~/Desktop/downloads"))
  puts "Mappen finns på skrivbordet"
end
```

Man kan också använda `Dir.exist?()` tillsammans med andra metoder som till exempel `Dir.glob()` för att kontrollera om en mapp innehåller vissa filer eller mönster.

## Se även
- [Dir-class (Ruby 3.0.0)](https://ruby-doc.org/core-3.0.0/Dir.html)
- [File-class (Ruby 3.0.0)](https://ruby-doc.org/core-3.0.0/File.html)
- [Working with Directories in Ruby (SitePoint)](https://www.sitepoint.com/working-with-directories-in-ruby/)