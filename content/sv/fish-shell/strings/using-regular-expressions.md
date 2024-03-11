---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:57.465657-07:00
description: "Regulj\xE4ra uttryck (regex) i Fish Shell g\xF6r det m\xF6jligt f\xF6\
  r dig att s\xF6ka, matcha och manipulera str\xE4ngar baserat p\xE5 specifika m\xF6\
  nster. Programmerare\u2026"
lastmod: '2024-03-11T00:14:11.725761-06:00'
model: gpt-4-0125-preview
summary: "Regulj\xE4ra uttryck (regex) i Fish Shell g\xF6r det m\xF6jligt f\xF6r dig\
  \ att s\xF6ka, matcha och manipulera str\xE4ngar baserat p\xE5 specifika m\xF6nster.\
  \ Programmerare\u2026"
title: "Att anv\xE4nda regulj\xE4ra uttryck"
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
