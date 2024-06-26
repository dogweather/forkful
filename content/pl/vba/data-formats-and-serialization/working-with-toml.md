---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:06:34.422093-07:00
description: "Jak to zrobi\u0107: Praca z TOML w VBA wi\u0105\u017Ce si\u0119 z parsowaniem\
  \ pliku TOML w celu odczytania konfiguracji lub ustawie\u0144 do projektu VBA. VBA\
  \ nie ma wbudowanego\u2026"
lastmod: '2024-03-13T22:44:35.257508-06:00'
model: gpt-4-0125-preview
summary: "Praca z TOML w VBA wi\u0105\u017Ce si\u0119 z parsowaniem pliku TOML w celu\
  \ odczytania konfiguracji lub ustawie\u0144 do projektu VBA."
title: Praca z TOML
weight: 39
---

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
