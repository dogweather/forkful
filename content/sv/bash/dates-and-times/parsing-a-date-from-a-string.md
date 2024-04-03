---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:47.561112-07:00
description: "Hur: Bash i sig \xE4r ganska begr\xE4nsat i direkt datumtolkningsf\xF6\
  rm\xE5ga, och f\xF6rlitar sig ofta p\xE5 externa verktyg som `date` och `awk` f\xF6\
  r mer sofistikerad\u2026"
lastmod: '2024-03-13T22:44:38.091594-06:00'
model: gpt-4-0125-preview
summary: "Bash i sig \xE4r ganska begr\xE4nsat i direkt datumtolkningsf\xF6rm\xE5\
  ga, och f\xF6rlitar sig ofta p\xE5 externa verktyg som `date` och `awk` f\xF6r mer\
  \ sofistikerad manipulation."
title: "Analysera ett datum fr\xE5n en str\xE4ng"
weight: 30
---

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
