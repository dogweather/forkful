---
date: 2024-01-20 17:40:05.601096-07:00
description: "Tempor\xE4ra filer \xE4r tempor\xE4ra lagerplatser f\xF6r data som anv\xE4\
  nds under programk\xF6rningen och som ofta raderas efter\xE5t. Programmerare skapar\
  \ dem f\xF6r att\u2026"
lastmod: '2024-03-11T00:14:11.759925-06:00'
model: gpt-4-1106-preview
summary: "Tempor\xE4ra filer \xE4r tempor\xE4ra lagerplatser f\xF6r data som anv\xE4\
  nds under programk\xF6rningen och som ofta raderas efter\xE5t. Programmerare skapar\
  \ dem f\xF6r att\u2026"
title: "Skapa en tempor\xE4r fil"
---

{{< edit_this_page >}}

## Vad & Varför?
Temporära filer är temporära lagerplatser för data som används under programkörningen och som ofta raderas efteråt. Programmerare skapar dem för att hantera data volatilt, undvika att kladda ned hårddisken med testdata och för att skydda privat information.

## Hur gör man:
Att skapa en temporär fil i Fish Shell är enkelt. Använd `mktemp`-kommandot:

```Fish Shell
set tempfile (mktemp)
echo "Det här är en tillfällig fil" > $tempfile
cat $tempfile
# Output: Det här är en tillfällig fil
```

För att sedan städa upp och ta bort den temporära filen:

```Fish Shell
rm $tempfile
# Kontrollera att filen är borttagen
if test -f $tempfile
    echo "Filen finns kvar."
else
    echo "Filen är bortagen."
end
# Output: Filen är bortagen.
```

## Djupdykning:
Historiskt har temporära filer varit ett sätt att minska belastningen på huvudminnet. Förr i tiden när minnet var dyrt och begränsat, var temporära filer ännu mer kritiska.

Det finns alternativ till `mktemp`, som att manuellt skapa unika filnamn med tidsstämplar eller slumpmässiga nummer, men dessa metoder medför större risk för namnkollisioner.

Fish Shell använder systemets `mktemp` för att skapa säkra temporära filer utan större chans för konflikter. Fish's syntax gör processen kort och kraftfull.

## Se även:
- Fish Shell dokumentation om inbyggda kommandon: https://fishshell.com/docs/current/cmds/mktemp.html
- Artiklar om säkerhet och bästa praxis för temporära filer.
- Guide till filhantering i Unix-baserade system: https://en.wikipedia.org/wiki/Filesystem_Hierarchy_Standard
