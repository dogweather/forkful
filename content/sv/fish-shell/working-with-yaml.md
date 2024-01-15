---
title:                "Arbeta med yaml"
html_title:           "Fish Shell: Arbeta med yaml"
simple_title:         "Arbeta med yaml"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/working-with-yaml.md"
---

{{< edit_this_page >}}

## Varför

YAML är en filformat som används för att strukturera data på ett läs- och skrivbart sätt. Det används ofta inom programmering för att konfigurera och konfigurera applikationer eller för att hantera data på ett organiserat sätt.

## Hur man använder YAML i Fish Shell

Fisk Shell har inbyggd stöd för att hantera YAML-filer. För att läsa in en YAML-fil i Fish Shell, använd kommandot `yq` följt av filnamnet:

```Fish Shell
yq filnamn.yml
```

Om du vill göra ändringar i YAML-filen, kan du använda `yq` tillsammans med pipelining. Till exempel, om du vill lägga till ett nytt objekt i YAML-filen, kan du göra det med:

```Fish Shell
echo "nytt_objekt:
    - namn: John
      ålder: 30" | yq -iy filnamn.yml
```

Outputen kommer att färdigställas i YAML-format och skrivas till filen. Detta gör det enkelt att ändra och uppdatera YAML-filer direkt från Fish Shell.

## Djupdykning

Förutom att läsa och ändra i YAML-filer, kan du också använda Fish Shell för att skapa helt nya YAML-filer. Med hjälp av kommandot `yq` tillsammans med `for`-loopen i Fish Shell, kan du generera YAML-filer baserade på befintliga mallar eller data.

Till exempel:

```Fish Shell
for rad i (seq 1 5)
    echo "objekt_$rad:
        - namn: Anonym
          ålder: $rad" | yq -yiy ny_filspec.yml
end
```

Genom att utnyttja de många funktionerna i Fish Shell och kommandot `yq`, kan du utföra många olika uppgifter med YAML-filer, från att läsa och ändra till att skapa helt nya.

## Se även

- [YAML officiella webbplats](https://yaml.org/)
- [YAML Wikipedia](https://en.wikipedia.org/wiki/YAML)
- [Fish Shell dokumentation](https://fishshell.com/docs/current/index.html)