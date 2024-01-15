---
title:                "Arbeta med yaml"
html_title:           "Ruby: Arbeta med yaml"
simple_title:         "Arbeta med yaml"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/working-with-yaml.md"
---

{{< edit_this_page >}}

# Varför

Om du arbetar med webbutveckling, databaser eller mjukvarutillämpningar är du förmodligen bekant med YAML. Det är ett lättläst dataformat som används för att lagra och överföra data. Att kunna hantera YAML är en viktig färdighet inom programmering, särskilt inom Ruby.

# Så här gör du

För att hantera YAML i Ruby behöver du först installera en YAML-paket. Detta kan enkelt göras genom att köra följande kommando i din terminal:

```
gem install yaml
```

När det är installerat kan du använda funktionen `require` för att importera YAML i ditt Ruby-program:

```
require 'yaml'
```

För att läsa in en YAML-fil och konvertera den till en Ruby-hash används följande kod:

```
yaml_hash = YAML.load_file('file_name.yml')
```

För att skriva en YAML-fil från en befintlig hash i Ruby används följande kod:

```
yaml_string = YAML.dump(hash)
File.open('file_name.yml', 'w') { |file| file.write(yaml_string) }
```

Deep Dive

En YAML-fil består av olika nyckel-värde-par som separeras av kolon och indenterade underkategorier som består av listor eller andra nyckel-värde-par. Det är ett enkelt och läsbart format som är användbart för att lagra konfigurationsdata eller andra typer av datastrukturer.

När du arbetar med YAML i Ruby är det viktigt att känna till att YAML-filen automatiskt konverteras till en hash. Detta betyder att om du behöver åtkomst till specifika värden i filen, kan du använda nycklar och metoder för att navigera genom hashen och hämta värdet.

# Se även

- [YAML.org](https://yaml.org/)
- [YAML Tutorial](https://www.codecademy.com/learn/learn-yaml)
- [Ruby YAML Documentation](https://ruby-doc.org/stdlib-2.7.1/libdoc/yaml/rdoc/YAML.html)