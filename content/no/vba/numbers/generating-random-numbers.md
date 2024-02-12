---
title:                "Generere tilfeldige tall"
date:                  2024-02-01T21:54:07.826082-07:00
model:                 gpt-4-0125-preview
simple_title:         "Generere tilfeldige tall"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/vba/generating-random-numbers.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å generere tilfeldige tall i Visual Basic for Applications (VBA) gir muligheten til å simulere prosesser med elementer av sjanse eller variabilitet, som terningkast eller prøveutvalg. Programmerere bruker disse teknikkene for å utvikle modeller, spill eller simuleringer der forutsigbare utfall ville være urealistiske eller mindre nyttige.

## Hvordan:

I VBA brukes `Rnd`-funksjonen til å generere tilfeldige tall. Som standard genererer `Rnd` et enkeltpresisjon flyttall som er større enn eller lik 0 og mindre enn 1. Her er noen få trinn og eksempler for effektivt å utnytte tilfeldige tall:

1. **Enkelt Tilfeldig Tall:**
   For å generere et grunnleggende tilfeldig tall, trenger du bare å kalle `Rnd()`:

   ```vb
   Sub GenerateRandomNumber()
       Dim randomNumber As Single
       randomNumber = Rnd() ' Tilfeldig tall mellom 0 og 1
       MsgBox randomNumber
   End Sub
   ```

2. **Sette Seed:**
   `Randomize`-setningen initialiserer generatoren for tilfeldige tall, noe som kan være avgjørende for å sikre forskjellige utfall hver gang VBA-koden din kjører:

   ```vb
   Sub SeedRandomNumber()
       Randomize
       Dim randomNumber As Single
       randomNumber = Rnd()
       MsgBox randomNumber
   End Sub
   ```

3. **Generere Tall innenfor et Område:**
   Ofte vil man ha et tilfeldig tall innenfor et spesifikt område. Slik genererer du et tall mellom 1 og 100:

   ```vb
   Sub RandomNumberInRange()
       Randomize
       Dim randomNumber As Integer
       randomNumber = Int((100 * Rnd()) + 1) ' Tilfeldig tall mellom 1 og 100
       MsgBox randomNumber
   End Sub
   ```

### Eksempel på Resultat:
Etter å ha kjørt `RandomNumberInRange`, kan du se en meldingsboks som viser et tall slik som `45`.

## Dypdykk:

`Rnd`-funksjonen i VBA, som er enkel å bruke, genererer faktisk pseudo-tilfeldige tall basert på en deterministisk algoritme. Dette betyr at sekvensene av tall den produserer ikke er helt tilfeldige, men kan ofte være tilstrekkelige for vanlige oppgaver som trenger stokastiske prosesser.

Historisk sett dateres kapasiteten for generering av tilfeldige tall i VBA tilbake til tidlige versjoner av Basic, og har med tiden tilpasset seg for å inkludere funksjoner som `Randomize` for å forbedre tilfeldigheten ved å seede algoritmen med et utgangspunkt. Imidlertid, for applikasjoner som krever høye nivåer av tilfeldighet, som sikre kryptografiske operasjoner, er kanskje ikke VBA’s `Rnd` det beste verktøyet. Alternativer i mer robuste programmeringsmiljøer eller språk designet med tanke på kryptografi, som Pythons `secrets`-modul eller Javas `SecureRandom`, bør vurderes.

Til tross for sine begrensninger, fortsetter enkelheten og tilgjengeligheten av å generere tilfeldige tall i VBA å gjøre det til et verdifullt verktøy for et bredt spekter av lettere applikasjoner, simuleringsarbeid og pedagogiske formål.
