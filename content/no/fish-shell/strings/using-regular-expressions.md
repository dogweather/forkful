---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:51.311184-07:00
description: "Hvordan: Selv om Fish Shell i seg selv ikke har en innebygd kommando\
  \ for regex, bruker den effektivt eksterne kommandoer som `grep`, `sed` og `awk`\
  \ som\u2026"
lastmod: '2024-03-13T22:44:41.215083-06:00'
model: gpt-4-0125-preview
summary: "Selv om Fish Shell i seg selv ikke har en innebygd kommando for regex, bruker\
  \ den effektivt eksterne kommandoer som `grep`, `sed` og `awk` som st\xF8tter regex."
title: "Bruke regul\xE6re uttrykk"
weight: 11
---

## Hvordan:
Selv om Fish Shell i seg selv ikke har en innebygd kommando for regex, bruker den effektivt eksterne kommandoer som `grep`, `sed` og `awk` som støtter regex. Dette lar deg inkludere regex-operasjoner i skriptene dine.

### Grunnleggende Mønstersøk med `grep`
Søk etter linjer i en fil som matcher et mønster:

```fish
grep '^[0-9]+' myfile.txt
```

Denne kommandoen finner linjer som starter med ett eller flere sifre i `myfile.txt`.

### Ekstrahering & Erstatning med `sed`
Ekstraher telefonnumre fra en fil:

```fish
sed -n '/\([0-9]\{3\}\)-\([0-9]\{3\}\)-\([0-9]\{4\}\)/p' contacts.txt
```

Erstatt alle forekomster av "foo" med "bar" i `data.txt`:

```fish
sed 's/foo/bar/g' data.txt
```

### Bruk av `string` for Grunnleggende Regex
Fish Shell-kommandoen `string` støtter enkle regex-operasjoner som treff og erstatning:

Match et mønster i en streng:

```fish
echo "fish 3.1.2" | string match -r '3\.[0-9]+\.[0-9]+'
```
Output:
```
3.1.2
```

Erstatt sifre etter 'fish' med 'X.X.X':

```fish
echo "Welcome to fish 3.1.2" | string replace -ra '([fish]+\s)[0-9\.]+' '$1X.X.X'
```
Output:
```
Welcome to fish X.X.X
```

### Avansert Samsvarende med `awk`
Skriv ut den andre kolonnen med data der den første kolonnen matcher et spesifikt mønster:

```fish
awk '$1 ~ /^a[0-9]+$/ {print $2}' datafile
```

Denne kommandoen ser etter linjer i `datafile` der den første kolonnen starter med en "a" etterfulgt av ett eller flere sifre og skriver ut den andre kolonnen.

Ved å integrere disse eksterne kommandoene, kan programmerere i Fish Shell utnytte den fulle kraften av regulære uttrykk for komplekse tekstmanipuleringsoppgaver, noe som forbedrer shellens naturlige kapasiteter.
