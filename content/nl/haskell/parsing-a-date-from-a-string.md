---
title:                "Een datum uit een string parsen"
aliases:
- nl/haskell/parsing-a-date-from-a-string.md
date:                  2024-01-28T22:04:15.906524-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een datum uit een string parsen"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/haskell/parsing-a-date-from-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Een datum uit een string parsen betekent tekst omzetten naar een datum datatyp. Programmeurs moeten vaak gebruikersinvoer of de inhoud van tekstbestanden omzetten in gestructureerde data voor verwerking en manipulatie.

## Hoe:

Haskell biedt meerdere manieren om data te parsen, maar laten we ons concentreren op de `time` bibliotheek en een eenvoudig voorbeeld met `parseTimeM`. Zorg ervoor dat je het `time` pakket hebt geïnstalleerd.

```haskell
import Data.Time.Format (parseTimeM, defaultTimeLocale)

main :: IO ()
main = do
  let dateString = "2023-03-21"
  let parsedDate = parseTimeM True defaultTimeLocale "%Y-%m-%d" dateString :: IO (Maybe Day)
  
  resultaat <- parsedDate
  case resultaat of
    Just dag -> putStrLn $ "Geparste datum: " ++ show dag
    Nothing -> putStrLn "Datum parsen mislukt."

-- Uitvoer moet zijn: Geparste datum: 2023-03-21
```

## Diepere Duik

Historisch gezien is het parsen van data verschillend aangepakt in talen en bibliotheken, waarbij velen variaties gebruiken op de `strftime` patronen van C. De `time` bibliotheek van Haskell spiegelt deze aanpak voor consistentie. Alternatieven voor `time` zijn onder andere het gebruik van het `old-time` pakket, dat nu verouderd is, of externe bibliotheken zoals `thyme` of `chronos`.

Wat betreft implementatie, parsing in Haskell is type-veilig, vandaar het gebruik van `Maybe` in het voorbeeld om mislukkingen bij het parsen te behandelen. De functie `parseTimeM` maakt gebruik van type-inferentie om het retourtype te bepalen, wat het flexibel maakt. Het begrijpen van de formaatspecificators, zoals `%Y-%m-%d` voor jaar-maand-dag, is cruciaal.

Het sterke typesysteem van Haskell zorgt ervoor dat, eenmaal een datum is geparseerd, duidelijk en onmiskenbaar is wat voor type het is, waardoor runtime fouten gerelateerd aan datummanipulatie verminderd worden. Echter, deze striktheid betekent dat je rekening moet houden met gevallen waarin de invoer niet overeenkomt met het verwachte patroon, vandaar de patroonmatching op `Just` en `Nothing`.

## Zie Ook

- Haskell `time` bibliotheekdocumentatie: [https://hackage.haskell.org/package/time](https://hackage.haskell.org/package/time)
- "Learn You a Haskell" gids over data en tijden: [http://learnyouahaskell.com/](http://learnyouahaskell.com/) - (zoek naar de "Data.Time" sectie)
- Formaatspecificators voor `Data.Time.Format`: [https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Format.html#v:formatTime](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Format.html#v:formatTime)
