---
title:                "Utskrift av feilsøkingsdata"
aliases: - /no/vba/printing-debug-output.md
date:                  2024-02-01T21:58:32.653811-07:00
model:                 gpt-4-0125-preview
simple_title:         "Utskrift av feilsøkingsdata"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/vba/printing-debug-output.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å skrive ut feilsøkingsutdata i Visual Basic for Applications (VBA) innebærer å strategisk plassere utskriftssetninger i koden din for å vise variabelverdier, utførelsesflyt eller tilpassede feilsøkingsmeldinger. Denne teknikken er essensiell for feilsøking, da den gjør det mulig for programmerere å forstå oppførselen til koden sin under kjøring og identifisere uventet oppførsel eller feil.

## Hvordan:
I VBA er `Debug.Print`-setningen en arbeidshest for å skrive ut feilsøkingsinformasjon til umiddelbarvinduet i Visual Basic Editor (VBE). For å bruke denne funksjonen effektivt, må du ha umiddelbarvinduet synlig (Vis > Umiddelbart Vindu eller trykk `Ctrl+G` i VBE).

Her er et enkelt eksempel på bruk av `Debug.Print` for å vise verdien av en variabel og en tilpasset melding:

```basic
Sub PrintDebugInfo()
    Dim sampleVar As Integer
    sampleVar = 42
    Debug.Print "Verdien av sampleVar er: "; sampleVar
End Sub
```

Når du kjører denne subrutinen, vil umiddelbarvinduet vise:
```
Verdien av sampleVar er: 42
```

Du kan også bruke den til å spore flyten av kompleks betinget logikk ved å sette inn `Debug.Print`-setninger i ulike grener av koden din:

```basic
Sub CheckValue()
    Dim valueToCheck As Integer
    valueToCheck = 9
    
    If valueToCheck > 10 Then
        Debug.Print "Verdien er større enn 10."
    ElseIf valueToCheck < 10 And valueToCheck > 0 Then
        Debug.Print "Verdien er mellom 1 og 9."
    Else
        Debug.Print "Verdien er 10 eller mindre enn 1."
    End If
End Sub
```

Å kjøre `CheckValue` produserer:
```
Verdien er mellom 1 og 9.
```

Husk at utdata fra `Debug.Print` bare går til umiddelbarvinduet, noe som er ekstremt nyttig under utviklingsfasen, men vises ikke i noen brukerorienterte deler av en applikasjon.

## Dypdykk
Umiddelbarvinduet og `Debug.Print`-metoden har dype røtter i historien til Visual Basic for Applications, hvilket reflekterer utviklingen av feilsøkingspraksis over tid. Til å begynne med var feilsøking en mer tekstuell og mindre visuell prosess, med utviklere som sterkt avhang av utskriftssetninger for å forstå hva koden deres gjorde. Over årene, ettersom utviklingsmiljøer utviklet seg, gjorde også feilsøkingsverktøyene det, ved å introdusere breakpoints, watches og mer sofistikerte profileringverktøy som gir en mer interaktiv og umiddelbar innsikt i kodeoppførsel.

Ikke desto mindre er `Debug.Print` og umiddelbarvinduet fortsatt utrolig nyttig, spesielt for raske og skitne feilsøkingsøkter eller når man jobber med kode som er vanskelig å bryte inn i (som hendelseshåndterere). Med det sagt, er det viktig å anerkjenne at å stole ene og alene på utskriftssetninger for feilsøking i moderne programmering kan være mindre effektivt sammenlignet med å utnytte integrerte feilsøkere med breakpoint, watch, og stakkinspeksjonskapasiteter.

Selv om alternativer som loggerammeveker eller mer avanserte feilsøkingsverktøy tilbyr flere funksjoner og fleksibilitet, gjør enkelheten og umiddelbarheten til `Debug.Print` i VBA det til et verdifullt verktøy, spesielt for programmerere som overgår fra andre språk og som allerede er vant med utskriftsbaserte feilsøkingsteknikker. Imidlertid, ettersom de blir mer komfortable med VBA og Visual Basic Editor, kan utforskning av hele utvalget av tilgjengelige feilsøkingsverktøy føre til mer effektiv og effektiv problemløsning.
