---
title:                "Arbeiten mit TOML"
aliases: - /de/vba/working-with-toml.md
date:                  2024-02-01T22:06:18.544798-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arbeiten mit TOML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/vba/working-with-toml.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?

TOML, was für Toms Offensichtliche, Minimale Sprache steht, ist ein Daten-Serialisierungsformat, das hauptsächlich für Konfigurationsdateien verwendet wird. Programmierer nutzen TOML wegen seiner Lesbarkeit und einfachen Zuordnung zu Datenstrukturen, was eine unkomplizierte Konfiguration von Anwendungen in verschiedenen Programmierumgebungen, einschließlich Visual Basic for Applications (VBA), ermöglicht.

## Wie:

Die Arbeit mit TOML in VBA beinhaltet das Parsen der TOML-Datei, um Konfigurationen oder Einstellungen in Ihr VBA-Projekt zu lesen. VBA hat keine integrierte Unterstützung für TOML, daher verwenden Sie normalerweise einen Parser oder konvertieren TOML-Daten in ein Format, mit dem VBA leicht arbeiten kann, wie JSON oder XML. So parsen Sie manuell eine einfache TOML-Konfigurationsdatei:

1. **Beispiel-TOML-Datei** (`config.toml`):
```
title = "TOML-Beispiel"

[database]
server = "192.168.1.1"
ports = [ 8000, 8001, 8002 ]
connection_max = 5000
enabled = true
```

2. **VBA-Code zum Parsen von TOML**:

Es wird angenommen, dass der TOML-Inhalt in eine String-Variable `tomlStr` gelesen wird. Der folgende VBA-Code zeigt einen simplen Ansatz zum Parsen des Abschnitts `[database]`:

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
    
    'Beispiel für den Zugriff auf geparste Daten
    Debug.Print "Datenbankserver: "; config("database")("server")
End Function
```

3. **Beispielausgabe** (Sofortfenster):
```
Datenbankserver: 192.168.1.1
```

## Vertiefende Betrachtung

Die praktische Akzeptanz von TOML in der Entwicklergemeinschaft zeigt einen Trend zu einfacheren, menschenlesbareren Konfigurationsdateien auf, im Gegensatz zu den früher vorherrschenden XML. TOMLs Designphilosophie betont klare Semantik und zielt auf einfaches Parsen mit minimalem Overhead ab. In VBA erfordert der direkte Umgang mit TOML das manuelle Parsen oder den Einsatz externer Werkzeuge zur Konvertierung von TOML in ein VBA-freundlicheres Format aufgrund fehlender nativer Unterstützung. Obwohl diese manuelle Parsmethode einen grundlegenden Ansatz darstellt, könnten der Einsatz externer Bibliotheken oder Zwischenformate wie JSON robustere und fehlerresistentere Parsstrategien bieten. Angesichts der umfangreichen Integration von VBA mit Microsoft Office könnte die Umwandlung von TOML in JSON und die Verwendung der nativen JSON-Parsing-Fähigkeiten von VBA (wo anwendbar) oder von Drittanbieter-JSON-Parsen einen effizienteren Workflow bieten. Darüber hinaus sollten Programmierer mit der kontinuierlichen Entwicklung von Daten-Serialisierungsformaten auch YAML in Betracht ziehen, das wie TOML die Lesbarkeit für Menschen betont, aber verschiedene Kompromisse in Bezug auf Komplexität und Flexibilität bietet.
