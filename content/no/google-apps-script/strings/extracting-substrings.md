---
aliases:
- /no/google-apps-script/extracting-substrings/
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:53:06.334761-07:00
description: "\xC5 utvinne substringer inneb\xE6rer \xE5 ta en del av en streng -\
  \ i hovedsak \xE5 skape en ny streng fra en del av en eksisterende streng. Programmerere\
  \ gj\xF8r dette\u2026"
lastmod: 2024-02-18 23:08:53.468986
model: gpt-4-0125-preview
summary: "\xC5 utvinne substringer inneb\xE6rer \xE5 ta en del av en streng - i hovedsak\
  \ \xE5 skape en ny streng fra en del av en eksisterende streng. Programmerere gj\xF8\
  r dette\u2026"
title: Uttrekking av delstrenger
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å utvinne substringer innebærer å ta en del av en streng - i hovedsak å skape en ny streng fra en del av en eksisterende streng. Programmerere gjør dette av en rekke årsaker, inkludert dataanalyse, tekstmanipulering for brukergrensesnitt, eller behandling av inndata for forskjellige applikasjoner, noe som gjør substring-utvinning til et allsidig verktøy i enhver skriptverktøykasse.

## Hvordan:

I Google Apps Script, som er basert på moderne JavaScript, kan utvinning av substringer oppnås gjennom flere metoder, inkludert `substring()`, `substr()` og `slice()`. Hver har sine nyanser, men de tjener alle formålet med å trekke ut spesifiserte tegn fra en streng.

```javascript
// Eksempel med substring()
var str = "Hello, world!";
var result = str.substring(0, 5);
console.log(result); // Output: Hello

// Eksempel med substr()
var resultSubstr = str.substr(7, 5);
console.log(resultSubstr); // Output: world

// Eksempel med slice()
var resultSlice = str.slice(-6);
console.log(resultSlice); // Output: world!
```

Hver metode tar to argumenter: startposisjonen og, unntatt for `slice()` som kan ta negative indekser for å starte fra slutten, sluttposisjonen eller antall tegn å utvinne. Det er verdt å merke seg at den opprinnelige strengen forblir uendret etter disse operasjonene, da de returnerer nye strengverdier.

## Dypdykk

Historisk sett har JavaScript-metodene for å utvinne substringer vært en kilde til forvirring på grunn av deres lignende navn og funksjonalitet. Men, i Google Apps Script og moderne JavaScript, er `substring()` og `slice()` mest brukt, med `substr()` som anses for å være foreldet. Dette er viktig å merke seg for de som skriver fremtidssikker kode.

Hovedforskjellen mellom `substring()` og `slice()` er hvordan de håndterer negative indekser; `substring()` behandler negative indekser som 0, mens `slice()` kan akseptere en negativ indeks for å starte uttrekkingen fra slutten av strengen. Dette gjør `slice()` spesielt hendig for tilfeller hvor den eksakte lengden på strengen kanskje ikke er kjent, eller når man trenger å ekstrahere fra slutten.

Når man bestemmer hvilken metode å bruke for substring-utvinning, koker valget ofte ned til de spesifikke kravene til operasjonen (f.eks. om håndtering av negative indekser er fordelaktig) og personlige eller teamkode standarder. Mens det ikke finnes en "one-size-fits-all" beste praksis, kan forståelsen av de subtile forskjellene og ytelsesimplikasjonene bidra til å ta en informert beslutning.
