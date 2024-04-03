---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:27.383635-07:00
description: "\xC5 hente den n\xE5v\xE6rende datoen i programmering er en grunnleggende\
  \ oppgave som lar deg hente og manipulere systemets dato- og tidsdata. I scripting\
  \ og\u2026"
lastmod: '2024-03-13T22:44:41.238167-06:00'
model: gpt-4-0125-preview
summary: "\xC5 hente den n\xE5v\xE6rende datoen i programmering er en grunnleggende\
  \ oppgave som lar deg hente og manipulere systemets dato- og tidsdata."
title: "F\xE5 dagens dato"
weight: 29
---

## Hva & Hvorfor?
Å hente den nåværende datoen i programmering er en grunnleggende oppgave som lar deg hente og manipulere systemets dato- og tidsdata. I scripting og automatiseringsoppgaver er det essensielt for å generere tidsstempler, planlegge oppgaver og opprette logger.

## Hvordan:
Fish Shell bruker eksterne kommandoer som `date` for å få den nåværende datoen, noe som gir fleksibilitet til å formatere utdataen etter behov. Slik bruker du det:

```fish
# Vis den nåværende datoen i standardformatet
echo (date)

# Eksempel på utdata: Wed 25 Oct 2023 15:42:03 BST
```

For å tilpasse formatet på datoen, kan du bruke `+`-alternativet etterfulgt av format-spesifikatorer:

```fish
# Vis den nåværende datoen i YYYY-MM-DD format
echo (date "+%Y-%m-%d")

# Eksempel på utdata: 2023-10-25
```

For mer komplekse oppgaver, som å jobbe med tidsstempler eller utføre datoaritmetikk, stoler Fish Shell på eksterne verktøy som `date` på grunn av sin skripting natur. Her er et eksempel på å få den nåværende UNIX-tidsstempelet:

```fish
# Få det nåværende UNIX-tidsstempelet
echo (date "+%s")

# Eksempel på utdata: 1666710123
```

Og for å legge til en dag til den nåværende datoen ved hjelp av `date`:

```fish
# Legg til en dag til den nåværende datoen
echo (date -d "+1 day" "+%Y-%m-%d")

# Eksempel på utdata: 2023-10-26
```

Merk: Eksemplene bruker `date` kommandoinnstillinger som fungerer med GNU coreutils. Alternativer kan variere i andre miljøer som macOS, som bruker BSD date kommando som standard. Henvis alltid til `date --help` eller man-siden for detaljer spesifikke til ditt miljø.
