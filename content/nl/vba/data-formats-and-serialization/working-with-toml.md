---
title:                "Werken met TOML"
aliases:
- /nl/vba/working-with-toml.md
date:                  2024-02-01T22:06:34.680083-07:00
model:                 gpt-4-0125-preview
simple_title:         "Werken met TOML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/vba/working-with-toml.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

TOML, wat staat voor Tom's Obvious, Minimal Language, is een gegevensserialisatieformaat dat voornamelijk wordt gebruikt voor configuratiebestanden. Programmeurs gebruiken TOML vanwege de leesbaarheid en eenvoudige afbeelding op datastructuren, wat het rechtstreeks configureren van applicaties in verschillende programmeeromgevingen, inclusief Visual Basic for Applications (VBA), mogelijk maakt.

## Hoe te:

Werken met TOML in VBA betreft het parsen van het TOML-bestand om configuraties of instellingen in je VBA-project te lezen. VBA heeft geen ingebouwde ondersteuning voor TOML, dus je gebruikt meestal een parser of converteert TOML-gegevens naar een formaat waarmee VBA gemakkelijk kan werken, zoals JSON of XML. Hier is hoe je handmatig een eenvoudig TOML-configuratiebestand parseert:

1. **Voorbeeld van een TOML-bestand** (`config.toml`):
```
titel = "Voorbeeld van TOML"

[database]
server = "192.168.1.1"
poorten = [ 8000, 8001, 8002 ]
max_connectie = 5000
ingeschakeld = waar
```

2. **VBA-code om TOML te Parsen**:

Er vanuit gaande dat de TOML-inhoud is gelezen naar een tekenreeksvariabele `tomlStr`, demonstreert de volgende VBA-code een eenvoudige benadering om de sectie `[database]` te parsen:

```vb
Function ParseTOML(tomlStr As String)
    Dim lines() As String
    lines = Split(tomlStr, vbCrLf)
    
    Dim config As Object
    Set config = CreateObject("Scripting.Dictionary")
    Dim huidigeSectie As String
    huidigeSectie = ""
    
    Dim i As Integer
    For i = 0 To UBound(lines)
        Dim regel As String
        regel = Trim(lines(i))
        If InStr(regel, "[") > 0 And InStr(regel, "]") > 0 Then
            huidigeSectie = Mid(regel, 2, Len(regel) - 2)
            Set config(huidigeSectie) = CreateObject("Scripting.Dictionary")
        ElseIf InStr(regel, "=") > 0 Then
            Dim delen() As String
            delen = Split(regel, "=")
            Dim sleutel As String
            sleutel = Trim(delen(0))
            Dim waarde As String
            waarde = Trim(delen(1))
            config(huidigeSectie)(sleutel) = waarde
        End If
    Next i
    
    'Voorbeeld om geparseerde gegevens te benaderen
    Debug.Print "Database Server: "; config("database")("server")
End Function
```

3. **Voorbeeld van Uitvoer** (Onmiddellijk Venster):
```
Database Server: 192.168.1.1
```

## Diepgaande Verkenning

De praktische acceptatie van TOML in de ontwikkelaarsgemeenschap laat een trend zien richting eenvoudigere, beter leesbare configuratiebestanden, in contrast met het voorheen gangbare XML. De ontwerpfilosofie van TOML benadrukt heldere semantiek en streeft naar eenvoudig parsen met minimale overhead. In VBA houdt direct omgaan met TOML manueel parsen in of het gebruik van externe hulpmiddelen om TOML naar een meer VBA-vriendelijk formaat te converteren vanwege de afwezigheid van native ondersteuning. Hoewel deze handmatige parseermethode een fundamentele benadering laat zien, kunnen externe bibliotheken of tussenformaten zoals JSON robuustere en foutbestendigere parsingsstrategieën bieden. Gezien de uitgebreide integratie van VBA met Microsoft Office, kan het converteren van TOML naar JSON en het gebruik van de native JSON-parseermogelijkheden van VBA (waar van toepassing) of externe JSON-parsers een meer gestroomlijnde workflow bieden. Bovendien moeten programmeurs met de continue evolutie van gegevensserialisatieformaten ook YAML overwegen, dat net als TOML menselijke leesbaarheid benadrukt, maar verschillende afwegingen biedt wat betreft complexiteit en flexibiliteit.
