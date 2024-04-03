---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:01:00.046372-07:00
description: "Hvordan: I VBA er det flere tiln\xE6rminger for \xE5 fjerne anf\xF8\
  rselstegn fra en streng. Her er et enkelt eksempel ved bruk av `Replace`-funksjonen,\
  \ som s\xF8ker\u2026"
lastmod: '2024-03-13T22:44:40.605078-06:00'
model: gpt-4-0125-preview
summary: "I VBA er det flere tiln\xE6rminger for \xE5 fjerne anf\xF8rselstegn fra\
  \ en streng."
title: Fjerne sitatmerker fra en streng
weight: 9
---

## Hvordan:
I VBA er det flere tilnærminger for å fjerne anførselstegn fra en streng. Her er et enkelt eksempel ved bruk av `Replace`-funksjonen, som søker etter en spesifikk delstreng (i dette tilfellet, et anførselstegn) i en streng og erstatter den med en annen delstreng (en tom streng ved fjerning).

```basic
Sub RemoveQuotesExample()
    Dim originalString As String
    originalString = "'Dette' er en ""test"" streng."
    
    ' Fjern enkle anførselstegn
    originalString = Replace(originalString, "'", "")
    
    ' Fjern doble anførselstegn
    originalString = Replace(originalString, Chr(34), "")
    
    Debug.Print originalString 'Utdata: Dette er en test streng.
End Sub
```

Merk at for doble anførselstegn bruker vi `Chr(34)` fordi et dobbelt anførselstegn er ASCII-tegn 34. Dette er nødvendig siden doble anførselstegn også brukes for å angi strenglitteraler i VBA.

For mer nyanserte scenarioer hvor anførselstegn kan være en del av nødvendig format (f.eks. inne i et sitert ord), kan mer sofistikert logikk, kanskje ved bruk av Regex eller parsing tegn for tegn, være nødvendig.

## Dykk dypere
VBA, som er en grunnpilar i å automatisere oppgaver innen Microsoft Office-pakken, tilbyr et rikt sett av funksjoner for strengmanipulering, med `Replace` som en av de mest brukte. Denne funksjonen, derimot, er bare begynnelsen på hva som kan oppnås med VBA når det gjelder strengmanipulering.

Historisk sett har VBA fra sine forgjengere lagt vekt på enkelhet for kontorautomasjonsoppgaver, derav den enkle implementeringen av funksjoner som `Replace`. Imidlertid, for moderne programmeringsoppgaver, spesielt de som involverer kompleks strengmanipulasjoner eller sanering, kan VBA vise sine begrensninger.

I slike tilfeller kan programmerere ty til å kombinere VBA med regulære uttrykk (via `VBScript_RegExp_55.RegExp`-objektet) for mer fleksibilitet og kraft i parsing og manipulering av strenger. Denne tilnærmingen introduserer imidlertid ytterligere kompleksitet og krever en solid forståelse av regex-mønstre, som kanskje ikke er egnet for alle brukere.

Til tross for sine begrensninger, dekker VBA sin `Replace`-funksjon effektivt mange vanlige scenarioer som involverer fjerning av anførselstegn fra strenger. Den tjener som en rask og enkel løsning for de fleste behov for strengmanipulering uten å dykke inn i det mer komplekse regex-territoriet. For de som når grensene for hva `Replace` og andre grunnleggende strengfunksjoner kan gjøre, kan utforsking av regex innen VBA eller å vurdere et mer robust språk tilpasset komplekse strengoperasjoner være de neste beste stegene.
