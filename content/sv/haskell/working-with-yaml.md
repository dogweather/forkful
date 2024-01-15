---
title:                "Arbeta med yaml"
html_title:           "Haskell: Arbeta med yaml"
simple_title:         "Arbeta med yaml"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/working-with-yaml.md"
---

{{< edit_this_page >}}

## Varför
Om du behöver arbeta med YAML-filer, är Haskell ett kraftfullt och effektivt val för att hantera dem. Det är också ett bra sätt att lära sig funktionell programmering genom praktisk kodning.

## Så här gör du
Först måste du installera Haskell på din dator. Sedan kan du använda följande kod för att läsa in och tolka en YAML-fil:

```Haskell
import Data.Yaml

main :: IO ()
main = do
  -- Läs in YAML-fil
  yamlFile <- readFile "exempel.yaml"
  -- Tolkar YAML-filen och returnerar en Maybe-sträng
  let result = decodeEither' yamlFile :: Either ParseException String
  case result of
    Left err -> putStrLn $ "Fel vid tolkning av YAML-fil: " ++ show err
    Right str -> putStrLn $ "YAML-fil tolkad som: " ++ str
```

Detta kommer att läsa in en YAML-fil "exempel.yaml", tolka den och skriva ut det tolkade resultatet. Om något går fel kommer ett felmeddelande att skrivas ut istället. 

Om du vill skriva en YAML-fil istället kan du använda följande kod:

```Haskell
import Data.Yaml

main :: IO ()
main = do
  -- Skapa ett värde för att konvertera till YAML
  let person = [("name" :: "John), ("age" :: 30)]
  -- Konvertera personvärde till YAML
  let yaml = encode person
  -- Skriv YAML till en fil
  writeFile "ny_person.yaml" yaml
```

Detta kommer att skapa en YAML-fil "ny_person.yaml" med det inmatade personvärdet i det kodade formatet.

## Djupdykning
Haskell har en robust YAML-parsning och serialisering med hjälp av biblioteket "yaml". Det ger dig möjlighet att läsa och skriva YAML-filer samt konvertera dem till och från Haskell-värden.

YAML innehåller flera nyckelfunktioner som inte finns i andra filformat, till exempel återanvändbara ankarpunkter och alias för att återanvända data, vilket gör det till ett flexibelt val för konfigurationer och datastrukturer.

## Se även
- Haskell dokumentation för YAML: https://hackage.haskell.org/package/yaml
- YAML officiell webbplats: https://yaml.org/
- En snabb intro till YAML: https://learnxinyminutes.com/docs/yaml/