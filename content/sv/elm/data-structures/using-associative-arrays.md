---
aliases:
- /sv/elm/using-associative-arrays/
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:11:01.848732-07:00
description: "Associativa arrayer, eller som Elm kallar dem, Ordb\xF6cker, mappar\
  \ nycklar till v\xE4rden p\xE5 ett s\xE4tt som g\xF6r uppslag, infogning och borttagning\
  \ av v\xE4rden\u2026"
lastmod: 2024-02-18 23:08:51.703159
model: gpt-4-0125-preview
summary: "Associativa arrayer, eller som Elm kallar dem, Ordb\xF6cker, mappar nycklar\
  \ till v\xE4rden p\xE5 ett s\xE4tt som g\xF6r uppslag, infogning och borttagning\
  \ av v\xE4rden\u2026"
title: "Att anv\xE4nda associativa arrayer"
---

{{< edit_this_page >}}

## Vad & Varför?

Associativa arrayer, eller som Elm kallar dem, Ordböcker, mappar nycklar till värden på ett sätt som gör uppslag, infogning och borttagning av värden supersmidigt. De är din tillgång när du behöver hålla reda på saker utan en strikt ordning, som användarpreferenser eller inventarielistor.

## Hur man gör:

I Elm arbetar du med Ordböcker i `Dict`-modulen, så låt oss dyka in i ett snabbt exempel:

```Elm
import Dict exposing (Dict)

-- Initiera en ordbok med String-nycklar och Int-värden
exampleDict : Dict String Int
exampleDict = Dict.fromList [("apple", 5), ("banana", 2), ("orange", 8)]

-- Lägga till eller uppdatera ett värde
updatedDict = Dict.insert "grape" 10 exampleDict

-- Hämta ett värde (observera Maybe-typen, eftersom nyckeln kanske inte finns)
fruitCount : Maybe Int
fruitCount = Dict.get "apple" updatedDict

-- Ta bort ett nyckel-värde-par
finalDict = Dict.remove "banana" updatedDict

-- Konvertera en ordbok tillbaka till en lista
dictToList = Dict.toList finalDict
```

Exempelutdata när man visar `dictToList`:

```Elm
[("apple", 5), ("grape", 10), ("orange", 8)]
```

Detta demonstrerar de grundläggande operationerna: att skapa, uppdatera, komma åt och iterera över en Ordbok.

## Fördjupning

Ordböcker i Elm använder internt en struktur som är känd som ett AVL-träd - en typ av självbalanserande binärt sökträd. Detta val balanserar mellan att säkerställa att operationer som insert, get och remove har bra prestanda (logaritmisk tidskomplexitet) och att bibehålla enkelhet i hanteringen av data.

Trots styrkorna med Elm:s `Dict`, är det inte en lösning som passar för allt. För samlingar som är ordnade eller behöver itereras över sekventiellt kan List eller Array vara mer lämpliga. Dessutom, när man arbetar med en fast uppsättning kända nycklar, kan att använda sig av anpassade typer (Elms version av enums) erbjuda mer typsäkerhet och tydligare avsikter i din kod.

I Elm-ekosystemet erbjuder `Dict` ett pålitligt sätt att hantera samlingar av nyckel-värde-par där nycklarna är unika och ordningen inte spelar någon roll. Medan nyare eller mer sofistikerade strukturer kan framkomma, förblir `Dict`-modulen ett grundläggande verktyg i Elmutvecklarens verktygslåda för dess enkelhet och effektivitet i hanteringen av associativa arrayer.
