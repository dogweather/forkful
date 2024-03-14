---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:47.131325-07:00
description: "\xC5 skrive til en tekstfil i Fish Shell lar deg lagre data vedvarende,\
  \ noe som gj\xF8r det enkelt \xE5 hente eller manipulere data enten av samme Fish-script\u2026"
lastmod: '2024-03-13T22:44:41.245920-06:00'
model: gpt-4-0125-preview
summary: "\xC5 skrive til en tekstfil i Fish Shell lar deg lagre data vedvarende,\
  \ noe som gj\xF8r det enkelt \xE5 hente eller manipulere data enten av samme Fish-script\u2026"
title: Skrive en tekstfil
---

{{< edit_this_page >}}

## Hva & hvorfor?

Å skrive til en tekstfil i Fish Shell lar deg lagre data vedvarende, noe som gjør det enkelt å hente eller manipulere data enten av samme Fish-script eller andre programmer. Programmerere gjør dette for logging, lagring av konfigurasjonsinnstillinger eller eksportering av data for videre behandling.

## Hvordan:

For å skrive til en tekstfil i Fish, kan du bruke `echo`-kommandoen kombinert med omdirigeringsoperatorer. Det finnes ikke populære tredjepartsbiblioteker spesifikt for filskriving i Fish, ettersom skallets innebygde kommandoer er greie og effektive for dette formålet.

### Skrive tekst til en ny fil eller overskrive en eksisterende fil:
```fish
echo "Hallo, Fish Shell!" > output.txt
```
Denne kommandoen skriver "Hallo, Fish Shell!" til `output.txt`, skaper filen hvis den ikke eksisterer, eller overskriver den hvis den gjør det.

### Legge til tekst i en eksisterende fil:
Hvis du vil legge til tekst i slutten av en eksisterende fil uten å fjerne dens nåværende innhold, bruk tilleggsoperatøren `>>`:
```fish
echo "Legger til ny linje i filen." >> output.txt
```

### Skrive flere linjer:
Du kan skrive flere linjer til en fil ved å bruke echo med et nymåltegn `\n`, eller du kan kjede sammen flere echo-kommandoer ved å bruke semikolon:
```fish
echo "Første Linje\nAndre Linje" > output.txt
# ELLER
echo "Første Linje" > output.txt; echo "Andre Linje" >> output.txt
```

### Eksempel på utdata:
For å se innholdet av `output.txt` etter å ha kjørt kommandoene ovenfor, bruk `cat`-kommandoen:
```fish
cat output.txt
```
```plaintext
Første Linje
Andre Linje
```
Å erstatte eller legge til tekster som vist manipulerer filinnholdet etter dine krav, noe som demonstrerer enkle, men kraftfulle måter å arbeide med tekstfiler i Fish Shell på.
