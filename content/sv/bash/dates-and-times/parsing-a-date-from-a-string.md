---
aliases:
- /sv/bash/parsing-a-date-from-a-string/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:47.561112-07:00
description: "Att tolka ett datum fr\xE5n en str\xE4ng i Bash inneb\xE4r att extrahera\
  \ och konvertera datuminformation fr\xE5n textdata till ett format som Bash kan\
  \ manipulera\u2026"
lastmod: 2024-02-18 23:08:51.970320
model: gpt-4-0125-preview
summary: "Att tolka ett datum fr\xE5n en str\xE4ng i Bash inneb\xE4r att extrahera\
  \ och konvertera datuminformation fr\xE5n textdata till ett format som Bash kan\
  \ manipulera\u2026"
title: "Analysera ett datum fr\xE5n en str\xE4ng"
---

{{< edit_this_page >}}

## Vad & Varför?

Att tolka ett datum från en sträng i Bash innebär att extrahera och konvertera datuminformation från textdata till ett format som Bash kan manipulera eller använda för vidare processer. Detta är ett vanligt krav i skriptning för uppgifter som loggfilsanalys, organisering av filer baserat på datumstämplar eller automatiserad rapportering, vilket gör det till en viktig färdighet för programmerare att hantera och använda tidsdata effektivt.

## Hur:

Bash i sig är ganska begränsat i direkt datumtolkningsförmåga, och förlitar sig ofta på externa verktyg som `date` och `awk` för mer sofistikerad manipulation. Här är hur du kan tolka ett specifikt format och sedan använda det med `date`-kommandot för att konvertera det eller utföra operationer.

**Exempel 1:** Extrahera en datumsträng och konvertera den till ett annat format.

Antag att du har ett datum i formatet `yyyy-mm-dd` och du vill konvertera det till `dd-mm-yyyy`.

```bash
original_date="2023-04-01"
formatted_date=$(date -d $original_date '+%d-%m-%Y')

echo $formatted_date
```

**Exempel på utdata:**
```
01-04-2023
```

Detta använder `date`-kommandot med `-d`-alternativet för att specificera datumsträngen som input, och `+%d-%m-%Y` för att formatera utdatan.

**Exempel 2:** Använda `awk` för att tolka ett datum från en strukturerad textlinje och konvertera det.

Antag att du har en loggfilsrad:

```
2023-04-01 12:00:00 Användare loggade in
```

Du kan extrahera och konvertera datumdelen med hjälp av `awk` och `date`.

```bash
log_line="2023-04-01 12:00:00 Användare loggade in"
date_part=$(echo $log_line | awk '{print $1}')
formatted_date=$(date -d $date_part "+%A, %B %d, %Y")

echo $formatted_date
```

**Exempel på utdata:**
```
Lördag, April 01, 2023
```

Detta exempel använder `awk` för att dela loggraden och extrahera datumdelen (`$1` representerar det första fältet avgränsat av mellanslag), och sedan används `date` för att omformatera det.

### Använda tredjepartverktyg

För mer komplex tolkning eller när man hanterar en mängd olika datumformat, kan tredjepartverktyg som `dateutils` vara mycket praktiska.

**Exempel med `dateutils`:**

Antag att du har en datumsträng i ett icke-standardformat, till exempel, `April 01, 2023`.

```bash
original_date="April 01, 2023"
formatted_date=$(dateconv -i "%B %d, %Y" -f "%Y-%m-%d" <<< $original_date)

echo $formatted_date
```

**Exempel på utdata:**
```
2023-04-01
```

Detta kommando använder `dateconv` från `dateutils`, specifierar inputformatet med `-i` och det önskade outputformatet med `-f`. `dateutils` stöder ett brett utbud av datum- och tidsformat, vilket gör det mycket mångsidigt för datumtolkningsuppgifter i Bash-skript.
