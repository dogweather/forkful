---
title:                "Bash: Arbeta med yaml"
simple_title:         "Arbeta med yaml"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/working-with-yaml.md"
---

{{< edit_this_page >}}

## Varför

Bash-programmering är ett kraftfullt sätt att automatisera uppgifter och processer på ditt datorsystem. Genom att arbeta med YAML-filer kan du enkelt konfigurera och strukturera data för dessa automatiska processer.

## Hur man gör det

För att arbeta med YAML i Bash, behöver du ett verktyg som heter yaml-bash, vilket kan installeras genom kommandot `pip install yaml-bash`. Sedan kan du använda följande syntax för att läsa och skriva till YAML-filer:

```
Bash

# Läs från en YAML-fil
read_yaml < <(cat example.yaml)

# Spara till en YAML-fil
write_yaml variable <(echo "$value") example.yaml
```

Låt oss se ett exempel på hur man använder yaml-bash för att skapa en YAML-fil och sedan läsa den för att hämta värden:

```
Bash

# Skapa en YAML-fil
cat > example.yaml << EOF
foo: bar
hello:
  - world
EOF

# Hämta värdet för "foo"
read_yaml < <(cat example.yaml)
echo $foo

# Hämta värdet för "hello"
read_yaml < <(cat example.yaml)
echo ${hello[0]}
```

Outputen från detta exempel ska vara:

```
bar
world
```

## Djupdykning

YAML är ett format för att strukturera data som är läsbart både för människor och datorer. Här är några saker att tänka på när du arbetar med YAML i Bash:

- Varje rad i en YAML-fil är en "nyckel: värde"-par, där nyckeln är åtskild från värdet med ett kolon.
- YAML-filer kan innehålla olika datatyper, inklusive strängar, nummer, listor och dictionarys.
- Om du vill ha fler än ett värde för en nyckel, använd en lista och separera värdena med ett bindestreck.
- Om du vill ha flera nyckel-värde-par i en dictionary, använd ett kolon för att separera nyckeln och värdet och separera dem med ett kommatecken.

Se till att läsa dokumentationen för yaml-bash för mer information och funktioner.

## Se även

- [yaml-bash dokumentation](https://github.com/Jack12816/yaml-bash)
- [Officiell YAML-specifikation](https://yaml.org/spec/)
- [Bash-programmering för nybörjare](https://linuxconfig.org/bash-scripting-tutorial-for-beginners)