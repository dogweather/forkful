---
title:                "Ruby: Att läsa en textfil"
simple_title:         "Att läsa en textfil"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Varför

Att kunna läsa en textfil är en grundläggande färdighet inom programmering och är särskilt användbar inom Ruby. Genom att läsa en textfil kan du enkelt bearbeta data och använda den i ditt program.

## Hur man gör det

För att läsa en textfil i Ruby använder du metoden `File.open`. Här är ett exempel på hur det kan se ut:

```Ruby
file = File.open("textfil.txt", "r")
puts file.read
```

I det här exemplet öppnar vi filen "textfil.txt" och läser sedan in hela filen med `read`-metoden. Sedan skriver vi ut innehållet på skärmen med `puts`.

Om du vill läsa en fil rad för rad kan du använda en `each`-loop:

```Ruby
file = File.open("textfil.txt", "r")
file.each do |line|
  puts line
end
```

Det här kommer att skriva ut varje rad i textfilen på en egen rad.

## Djupdykning

När du läser en textfil, är det viktigt att vara medveten om filens format och encoding. Om filen är i ett annat format eller encoding än ditt program förväntar sig, kan du hamna i problem.

För att undvika detta kan du specificera encodingen när du öppnar filen genom att lägga till ett tillval till `File.open`-metoden:

```Ruby
file = File.open("textfil.txt", "r:UTF-8")
```

I det här exemplet specificerar vi att filen används i UTF-8 encoding. Om din fil är i ett annat format, t.ex. ISO-8859-1, kan du använda:

```Ruby
file = File.open("textfil.txt", "r:ISO-8859-1")
```

Om du vill läsa in filen på olika ställen i ditt program kan du också använda metoden `File.readlines`. Det här kommer att läsa in hela filen och spara varje rad som ett element i en array:

```Ruby
lines = File.readlines("textfil.txt")
puts lines[3]  # Skriver ut den fjärde raden i filen
```

## Se även

- [Ruby dokumentation om File klassen](https://ruby-doc.org/core-2.7.1/File.html)
- [Skillshare kurs om using Ruby for programming](https://www.skillshare.com/search?query=ruby%20programming) (på engelska)
- [Ruby för nybörjare: Läsa och skriva till filer](https://www.rubyguides.com/2015/05/working-with-files-ruby/) (på engelska)