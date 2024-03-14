---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:07:12.180245-07:00
description: "\xC5 fjerne anf\xF8rselstegn fra en streng i Go handler om \xE5 eliminere\
  \ de ledende og avsluttende anf\xF8rselstegnene (`\"` eller `'`) fra en gitt streng.\u2026"
lastmod: '2024-03-13T22:44:40.252728-06:00'
model: gpt-4-0125-preview
summary: "\xC5 fjerne anf\xF8rselstegn fra en streng i Go handler om \xE5 eliminere\
  \ de ledende og avsluttende anf\xF8rselstegnene (`\"` eller `'`) fra en gitt streng.\u2026"
title: "Fjerner anf\xF8rselstegn fra en streng"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å fjerne anførselstegn fra en streng i Go handler om å eliminere de ledende og avsluttende anførselstegnene (`"` eller `'`) fra en gitt streng. Programmerere trenger ofte å utføre denne oppgaven for å sanere brukerinndata, analysere tekstdata mer effektivt eller forberede strenger for videre behandling som krever innhold uten anførselstegn.

## Hvordan:

Go tilbyr flere tilnærminger for å fjerne anførselstegn fra en streng, men en av de mest direkte metodene er å bruke `Trim` og `TrimFunc`-funksjonene som tilbys av `strings`-pakken. Slik gjør du det:

```go
package main

import (
	"fmt"
	"strings"
	"unicode"
)

func main() {
	quotedString := `"Dette er en 'sitert' streng"`

	// Bruker strings.Trim for å fjerne spesifikke sitater
	unquoted := strings.Trim(quotedString, `"'`)
	fmt.Println("Bruker strings.Trim:", unquoted)

	// Tilpasset tilnærming ved bruk av strings.TrimFunc for mer kontroll
	unquotedFunc := strings.TrimFunc(quotedString, func(r rune) bool {
		return r == '"' || r == '\''
	})
	fmt.Println("Bruker strings.TrimFunc:", unquotedFunc)
}
```

Dette eksemplet demonstrerer to tilnærminger for å fjerne både doble (`"`) og enkle (`'`) sitater. `strings.Trim`-funksjonen er enklere og fungerer godt når du vet nøyaktig hvilke tegn du skal fjerne. På den andre siden gir `strings.TrimFunc` mer fleksibilitet, og lar deg spesifisere en tilpasset funksjon for å bestemme hvilke tegn som blir fjernet. Eksempelutdata for ovenstående kode er:

```
Bruker strings.Trim: Dette er en 'sitert' streng
Bruker strings.TrimFunc: Dette er en 'sitert' streng
```

Begge metodene fjerner effektivt de ledende og avsluttende sitatene fra strengen.

## Dypdykk

Funksjonene `Trim` og `TrimFunc` fra `strings`-pakken er en del av Gos omfattende standardbibliotek, designet for å tilby kraftige, men likevel enkle strengmanipulasjonsevner uten behov for tredjepartspakker. Historisk sett stammer behovet for å håndtere og manipulere strenger effektivt fra Gos primære fokus på nettverksservere og datatolkere, hvor strengbehandling er en vanlig oppgave.

Et bemerkelsesverdig aspekt ved disse funksjonene er deres implementasjon basert på runes (Gos representasjon av et Unicode-kodepunkt). Dette designet gjør det mulig for dem å sømløst håndtere strenger som inneholder flerbyte-tegn, noe som gjør Gos tilnærming til strengmanipulering både robust og Unicode-vennlig.

Selv om direkte bruk av `Trim` og `TrimFunc` for å fjerne sitater er praktisk og ideomatisk i Go, er det verdt å nevne at for mer komplekse strengbehandlingsoppgaver (f.eks. nestede sitater, escapede sitater), kan regulære uttrykk (via `regexp`-pakken) eller manuell parsing tilby bedre løsninger. Imidlertid kommer disse alternativene med økt kompleksitet og ytelsesbetraktninger. Derfor, for enkel fjerning av sitater, slår de demonstrerte metodene en god balanse mellom enkelhet, ytelse og funksjonalitet.
