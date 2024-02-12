---
title:                "Att använda reguljära uttryck"
aliases:
- /sv/fish-shell/using-regular-expressions.md
date:                  2024-02-03T19:16:57.465657-07:00
model:                 gpt-4-0125-preview
simple_title:         "Att använda reguljära uttryck"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?

Reguljära uttryck (regex) i Fish Shell gör det möjligt för dig att söka, matcha och manipulera strängar baserat på specifika mönster. Programmerare använder regex för uppgifter som inputvalidering, parsing och textbehandling eftersom det erbjuder ett kompakt och kraftfullt sätt att specificera komplexa textmönster.

## Hur man gör:

Även om Fish Shell i sig inte har ett inbyggt kommando för regex, använder det effektivt externa kommandon som `grep`, `sed` och `awk` som stödjer regex, vilket gör att du kan integrera regex-operationer i dina skript.

### Grundläggande mönstermatchning med `grep`
Sök efter rader i en fil som matchar ett mönster:

```fish
grep '^[0-9]+' myfile.txt
```

Detta kommando hittar rader som börjar med en eller flera siffror i `myfile.txt`.

### Extrahera och Ersätta med `sed`
Extrahera telefonnummer från en fil:

```fish
sed -n '/\([0-9]\{3\}\)-\([0-9]\{3\}\)-\([0-9]\{4\}\)/p' contacts.txt
```

Ersätt alla förekomster av "foo" med "bar" i `data.txt`:

```fish
sed 's/foo/bar/g' data.txt
```

### Använd `string` för Grundläggande Regex
Fish Shells `string`-kommando stödjer enkla regex-operationer som matchning och ersättning:

Matcha ett mönster i en sträng:

```fish
echo "fish 3.1.2" | string match -r '3\.[0-9]+\.[0-9]+'
```
Utdata:
```
3.1.2
```

Ersätt siffror som följer 'fish' med 'X.X.X':

```fish
echo "Välkommen till fish 3.1.2" | string replace -ra '([fish]+\s)[0-9\.]+' '$1X.X.X'
```
Utdata:
```
Välkommen till fish X.X.X
```

### Avancerad Matchning med `awk`
Skriv ut den andra kolumnen av data där den första kolumnen matchar ett specifikt mönster:

```fish
awk '$1 ~ /^a[0-9]+$/ {print $2}' datafile
```

Detta kommando letar efter rader i `datafile` där den första kolumnen börjar med ett "a" följt av en eller flera siffror och skriver ut den andra kolumnen.

Genom att integrera dessa externa kommandon kan Fish Shell-programmerare dra full nytta av reguljära uttryck för komplex textmanipulering, vilket förstärker shellens inbyggda kapaciteter.
