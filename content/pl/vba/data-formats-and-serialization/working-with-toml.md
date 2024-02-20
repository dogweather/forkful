---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:06:34.422093-07:00
description: "TOML, co oznacza Tom's Obvious, Minimal Language, to format serializacji\
  \ danych g\u0142\xF3wnie stosowany w plikach konfiguracyjnych. Programi\u015Bci\
  \ wykorzystuj\u0105\u2026"
lastmod: 2024-02-19 22:04:54.385973
model: gpt-4-0125-preview
summary: "TOML, co oznacza Tom's Obvious, Minimal Language, to format serializacji\
  \ danych g\u0142\xF3wnie stosowany w plikach konfiguracyjnych. Programi\u015Bci\
  \ wykorzystuj\u0105\u2026"
title: Praca z TOML
---

{{< edit_this_page >}}

## Co i dlaczego?

TOML, co oznacza Tom's Obvious, Minimal Language, to format serializacji danych głównie stosowany w plikach konfiguracyjnych. Programiści wykorzystują TOML ze względu na jego czytelność i łatwość mapowania na struktury danych, co umożliwia proste konfigurowanie aplikacji w różnych środowiskach programowania, w tym w Visual Basic dla Aplikacji (VBA).

## Jak to zrobić:

Praca z TOML w VBA wiąże się z parsowaniem pliku TOML w celu odczytania konfiguracji lub ustawień do projektu VBA. VBA nie ma wbudowanego wsparcia dla TOML, więc zazwyczaj używa się parsera lub konwertuje dane TOML na format, z którym VBA może łatwo pracować, tak jak JSON lub XML. Oto jak ręcznie sparsować prosty plik konfiguracyjny TOML:

1. **Przykładowy plik TOML** (`config.toml`):
```
title = "TOML Example"

[database]
server = "192.168.1.1"
ports = [ 8000, 8001, 8002 ]
connection_max = 5000
enabled = true
```

2. **Kod VBA do parsowania TOML**:

Założenie, że zawartość TOML jest wczytana do zmiennej typu string `tomlStr`, poniższy kod VBA demonstruje uproszczone podejście do parsowania sekcji `[database]`:

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
    
    'Przykład dostępu do sparsowanych danych
    Debug.Print "Serwer bazy danych: "; config("database")("server")
End Function
```

3. **Przykładowy wynik** (Okno natychmiastowe):
```
Serwer bazy danych: 192.168.1.1
```

## Głębsze spojrzenie

Praktyczne przyjęcie TOML wśród deweloperów pokazuje trend w kierunku prostszych, bardziej czytelnych dla człowieka plików konfiguracyjnych, w przeciwieństwie do wcześniej dominującego XML. Filozofia projektowa TOML podkreśla jasną semantykę i ma na celu łatwe parsowanie przy minimalnym nakładzie pracy. W VBA obsługa TOML bezpośrednio wiąże się z ręcznym parsowaniem lub wykorzystaniem zewnętrznych narzędzi do konwersji TOML na format bardziej przyjazny dla VBA z powodu braku natywnej obsługi. Chociaż ta metoda ręcznego parsowania prezentuje podstawowe podejście, wykorzystanie zewnętrznych bibliotek czy pośrednich formatów, takich jak JSON, może zaoferować bardziej zaawansowane i odporne na błędy strategie parsowania. Mając na uwadze obszerną integrację VBA z Microsoft Office, konwersja TOML na JSON i wykorzystanie natywnych możliwości parsowania JSON VBA (gdzie to możliwe) lub zewnętrznych parserów JSON mogą zapewnić bardziej płynny przepływ pracy. Ponadto, z ciągłą ewolucją formatów serializacji danych, programiści powinni również rozważyć YAML, który podobnie jak TOML, kładzie nacisk na czytelność dla człowieka, ale oferuje inne kompromisy pod względem złożoności i elastyczności.
