---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:06:27.253492-07:00
description: "S\xE5 h\xE4r g\xF6r du: Att arbeta med TOML i VBA inneb\xE4r att tolka\
  \ TOML-filen f\xF6r att l\xE4sa in konfigurationer eller inst\xE4llningar till ditt\
  \ VBA-projekt. VBA har\u2026"
lastmod: '2024-03-13T22:44:37.768849-06:00'
model: gpt-4-0125-preview
summary: "Att arbeta med TOML i VBA inneb\xE4r att tolka TOML-filen f\xF6r att l\xE4\
  sa in konfigurationer eller inst\xE4llningar till ditt VBA-projekt."
title: Arbeta med TOML
weight: 39
---

## Så här gör du:
Att arbeta med TOML i VBA innebär att tolka TOML-filen för att läsa in konfigurationer eller inställningar till ditt VBA-projekt. VBA har inte inbyggt stöd för TOML, så du kommer vanligtvis att använda en parser eller konvertera TOML-data till ett format som VBA kan hantera enkelt, som JSON eller XML. Så här tolkar du manuellt en enkel TOML-konfigurationsfil:

1. **Exempel på TOML-fil** (`config.toml`):
```
title = "TOML Exempel"

[database]
server = "192.168.1.1"
ports = [ 8000, 8001, 8002 ]
maxconnection = 5000
enabled = true
```

2. **VBA-kod för att tolka TOML**:

Med antagandet att TOML-innehållet läses in i en strängvariabel `tomlStr`, visar följande VBA-kod en enkel metod för att tolka avsnittet `[database]`:

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
        End If
    Next i
    
    'Exempel för att komma åt tolkad data
    Debug.Print "Databasserver: "; config("database")("server")
End Function
```

3. **Exempel på utdata** (Omedelbart Fönster):
```
Databasserver: 192.168.1.1
```

## Fördjupning
Det praktiska mottagandet av TOML i utvecklargemenskapen visar en trend mot enklare, mer läsliga konfigurationsfiler, i kontrast till det tidigare rådande XML. TOML:s designfilosofi betonar tydliga semantik och syftar till enkel tolkning med minimal överbelastning. I VBA innebär direkt hantering av TOML manuell tolkning eller användning av externa verktyg för att konvertera TOML till ett mer VBA-vänligt format på grund av bristen på inbyggt stöd. Medan denna manuella tolkningsmetod visar en grundläggande strategi, kan användningen av externa bibliotek eller mellanformat som JSON erbjuda mer robusta och felresistenta tolkningsstrategier. Med tanke på VBA:s omfattande integration med Microsoft Office, kan konverteringen av TOML till JSON och användningen av VBA:s inbyggda JSON-parsingförmåga (där det är tillämpligt) eller tredjeparts JSON-parsers ge en mer strömlinjeformad arbetsflöde. Vidare, med den kontinuerliga utvecklingen av data-serialiseringsformat, bör programmerare också överväga YAML, som liksom TOML betonar läsbarhet men erbjuder olika avvägningar när det gäller komplexitet och flexibilitet.
