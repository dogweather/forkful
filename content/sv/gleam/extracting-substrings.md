---
title:                "Extrahera delsträngar"
html_title:           "Arduino: Extrahera delsträngar"
simple_title:         "Extrahera delsträngar"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Ordningsföljden av tecken inom en sträng benämns som substräng. Programmerare använder substrängar för att behandla delar av datan, utan att störa den totala informationen.

## Hur till:

För att extrahera substrängar i Gleam använder vi `slice` funktionen. Låt oss testa med en exempelsträng "Hej, världen!".

```Gleam
let min_sträng = "Hej, världen!"

let substräng = slice(min_sträng, 4, 10)
```
I det här fallet kommer `substräng` att vara ", värld".

## Djup Dykning

Idén att extrahera substrängar kommer inte enbart från Gleam. Den har djupa rötter i många andra programmeringsspråk som C och JavaScript. Som alternativ kan funktionen `split_at` användas för att dela strängen på en specifik position. De interna detaljerna involverar att kopiera tecken från en startposition till en slutposition.

## Se Även

Se mer om substrängar och strängmanipulation på Gleam's officiella dokumentation (https://hexdocs.pm/gleam_stdlib/latest/gleam/string/). Det finns även en hel del användbara informationskällor på StackOverflow (https://stackoverflow.com/).