---
date: 2024-01-27 16:20:46.274650-07:00
description: "Hvordan: La oss starte med en enkel oppgave: du \xF8nsker \xE5 erstatte\
  \ alle forekomster av \"oldtext\" med \"newtext\" i en fil som heter example.txt.\
  \ Slik gj\xF8r du\u2026"
lastmod: '2024-03-13T22:44:41.013907-06:00'
model: gpt-4-0125-preview
summary: La oss starte med en enkel oppgave.
title: "Redigering av filer p\xE5 stedet med CLI-enlinjerskommandoer"
weight: 32
---

## Hvordan:


### Erstatte tekst i en enkelt fil
La oss starte med en enkel oppgave: du ønsker å erstatte alle forekomster av "oldtext" med "newtext" i en fil som heter example.txt. Slik gjør du det:

```PowerShell
(Get-Content example.txt) -replace 'oldtext', 'newtext' | Set-Content example.txt
```

Dette enlinjerskriptet leser innholdet, utfører erstatningen og skriver innholdet tilbake til den opprinnelige filen.

### Redigere flere filer
Hva om du trenger å anvende samme endring på flere filer? Her er en tilnærming som bruker en løkke:

```PowerShell
Get-ChildItem *.txt | ForEach-Object {
  (Get-Content $_) -replace 'oldtext', 'newtext' | Set-Content $_
}
```

Dette kodeutdraget finner alle `.txt`-filer i den gjeldende mappen, og erstatter "oldtext" med "newtext" i hver av dem.

### Legge til innhold i begynnelsen eller slutten av filer
Det å legge til eller foran innhold kan også forenkles:

```PowerShell
# Foranstille
"New first line`n" + (Get-Content example.txt) | Set-Content example.txt

# Tilføye
(Get-Content example.txt) + "`nNew last line" | Set-Content example.txt
```

Her, vi enkelt legger til det nye innholdet før eller etter det eksisterende innholdet og lagrer det tilbake.

## Dypdykk
Historisk sett er redigering på stedet mer vanlig assosiert med Unix-verktøy som `sed` og `awk`. PowerShell, som er en nyere deltaker, inkluderer ikke en dedikert funksjon for redigering på stedet rett ut av boksen. Dette skyldes delvis dens designfilosofi, som fremhever viktigheten av objekter fremfor tekststrømmer, i motsetning til Unix-verktøy som behandler de fleste inndata som tekst.

Alternativer til PowerShell for denne oppgaven inkluderer å bruke tradisjonelle Unix-verktøy tilgjengelige på Windows gjennom Cygwin eller Windows Subsystem for Linux (WSL). Disse verktøyene gir ofte en mer konsis syntaks for redigering på stedet på grunn av deres tekstsentriske design.

Når det gjelder implementering, er det viktig å merke seg at PowerShells tilnærming innebærer å lese hele filen inn i minnet, gjøre endringer og deretter skrive den tilbake. Mens dette fungerer bra for moderat store filer, kan det bli ineffektivt for veldig store filer. I slike tilfeller kan man vurdere å bruke `.NET`-metoder direkte eller ty til alternative verktøy designet for strømming av store datamengder.

Til tross for disse betraktningene, gjør PowerShells fleksibilitet og omfattende funksjonssett det til et uvurderlig verktøy for å manipulere filer direkte fra kommandolinjen, spesielt for de som allerede er godt etablert i Windows-økosystemet eller håndterer plattformuavhengige miljøer.
