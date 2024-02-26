---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:06:37.111190-07:00
description: "TOML, som st\xE5r for Toms Opplagte, Minimale Spr\xE5k, er et data-serieliseringsformat\
  \ som hovedsakelig brukes for konfigurasjonsfiler. Programmerere benytter\u2026"
lastmod: '2024-02-25T18:49:38.826979-07:00'
model: gpt-4-0125-preview
summary: "TOML, som st\xE5r for Toms Opplagte, Minimale Spr\xE5k, er et data-serieliseringsformat\
  \ som hovedsakelig brukes for konfigurasjonsfiler. Programmerere benytter\u2026"
title: Arbeider med TOML
---

{{< edit_this_page >}}

## Hva & Hvorfor?

TOML, som står for Toms Opplagte, Minimale Språk, er et data-serieliseringsformat som hovedsakelig brukes for konfigurasjonsfiler. Programmerere benytter TOML for dets lesbarhet og enkle kartlegging til datastrukturer, noe som muliggjør enkel konfigurasjon av applikasjoner på tvers av forskjellige programmeringsmiljøer, inkludert Visual Basic for Applications (VBA).

## Hvordan:

Å jobbe med TOML i VBA involverer parsing av TOML-filen for å lese konfigurasjoner eller innstillinger inn i ditt VBA-prosjekt. VBA har ikke innebygd støtte for TOML, så du vil typisk bruke en parser eller konvertere TOML-data til et format som VBA kan jobbe med enkelt, som JSON eller XML. Her er hvordan du manuelt parser en enkel TOML-konfigurasjonsfil:

1. **Eksempel på TOML-fil** (`config.toml`):
```
tittel = "TOML Eksempel"

[database]
server = "192.168.1.1"
porter = [ 8000, 8001, 8002 ]
maks_tilkobling = 5000
aktivert = sant
```

2. **VBA-kode for å Parse TOML**:

Forutsatt at TOML-innholdet er lest inn i en strengvariabel `tomlStr`, demonstrerer følgende VBA-kode en enkel tilnærming til å parse `[database]`-seksjonen:

```vb
Function ParseTOML(tomlStr As String)
    Dim lines() As String
    lines = Split(tomlStr, vbCrLf)
    
    Dim config As Object
    Set config = CreateObject("Scripting.Dictionary")
    Dim currentSection As String
    currentSection = ""
    
    Dim i As Integer
    For i = 0 To UBound(lines)
        Dim line As String
        line = Trim(lines(i))
        If InStr(line, "[") > 0 And InStr(line, "]") > 0 Then
            currentSection = Mid(line, 2, Len(line) - 2)
            Set config(currentSection) = CreateObject("Scripting.Dictionary")
        ElseIf InStr(line, "=") > 0 Then
            Dim parts() As String
            parts = Split(line, "=")
            Dim key As String
            key = Trim(parts(0))
            Dim value As String
            value = Trim(parts(1))
            config(currentSection)(key) = value
        End IF
    Next i
    
    'Eksempel på tilgang til parsede data
    Debug.Print "Databaseserver: "; config("database")("server")
End Function
```

3. **Eksempel på Utdata** (Umiddelbart Vindu):
```
Databaseserver: 192.168.1.1
```

## Dypdykk

Den praktiske aksepten av TOML i utviklerfellesskapet viser en trend mot enklere, mer menneskelesbare konfigurasjonsfiler, i kontrast til det tidligere utbredte XML. TOMLs designfilosofi understreker klare semantikker og sikter mot grei parsing med minimal overhead. I VBA innebærer håndtering av TOML direkte parsing eller bruk av eksterne verktøy for å konvertere TOML til et mer VBA-vennlig format på grunn av mangelen på innebygd støtte. Selv om denne manuelle parsemetoden viser en grunnleggende tilnærming, kan bruk av eksterne biblioteker eller mellomformater som JSON tilby mer robuste og feilbestandige parsestrategier. Gitt VBAs omfattende integrasjon med Microsoft Office, kan konvertering av TOML til JSON og bruk av VBAs innebygde JSON-parsefunksjoner (der det er aktuelt) eller tredjeparts JSON-parsere, gi en mer strømlinjeformet arbeidsflyt. Videre, med den kontinuerlige utviklingen av data-serieliseringsformater, bør programmerere også vurdere YAML, som, lik TOML, legger vekt på menneskelig lesbarhet, men tilbyr forskjellige avveininger når det gjelder kompleksitet og fleksibilitet.
