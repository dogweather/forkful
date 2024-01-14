---
title:                "Ruby: Kontrollera om en mapp finns"
simple_title:         "Kontrollera om en mapp finns"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Varför

I programmering är det alltid viktigt att hantera olika scenarier och möjliga felaktiga beteenden som kan uppstå. En av dessa situationer är att kontrollera om en mapp eller katalog existerar på en viss sökväg. Detta är en viktig åtgärd för att säkerställa att vår kod inte kraschar eller ger felaktiga resultat.

# Så här gör du

Vi använder oss av Ruby-metoden `Dir.exist?` för att kontrollera om en mapp finns på en specifik sökväg. Detta returnerar en boolesk värde som antingen är sant om mappen existerar eller falskt om den inte gör det.

```Ruby
# Skapar en sökväg och kontrollerar om den existerar
path = "./min_mapp"
puts Dir.exist?(path) # Returnerar false

# Skapar en mapp och kontrollerar igen
Dir.mkdir(path)
puts Dir.exist?(path) # Returnerar true
```

Om vi vill göra en specifik åtgärd om mappen existerar eller inte, kan vi använda en `if`-sats tillsammans med `Dir.exist?`:

```Ruby
if Dir.exist?(path)
  puts "Mappen existerar redan"
else
  Dir.mkdir(path)
  puts "Mappen skapad"
end
```

# Djupdykning

Bortsett från metoden `Dir.exist?`, finns det också andra sätt att kontrollera om en mapp existerar. Till exempel kan vi använda oss av `File.exist?` som kontrollerar om en fil med ett visst namn existerar och returnerar samma booleska värde. Detta kan vara användbart om vi bara vill kontrollera en specifik fil inuti en mapp.

Det är också viktigt att notera att `Dir.exist?` endast kontrollerar om mappen finns på en specificerad sökväg. Om sökvägen innehåller flera undermappar, kommer denna metod inte kontrollera om alla undermappar också existerar. I sådana fall rekommenderas det att använda sig av en mer detaljerad kontroll, till exempel att använda `Dir.glob` för att få en lista över alla undermappar och sedan kontrollera varje enskild mapp.

# Se också

- [Ruby Dokumentation om `Dir.exist?`](https://ruby-doc.org/core/Dir.html#method-c-exist-3F)
- [Ruby Dokumentation om `File.exist?`](https://ruby-doc.org/core/File.html#method-c-exist-3F)
- [Tutorial: How to Check if a Directory Exists in Ruby](https://www.rubyguides.com/2019/10/ruby-check-if-directory-exists/)