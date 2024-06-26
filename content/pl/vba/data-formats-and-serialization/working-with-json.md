---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:05:57.870114-07:00
description: "Jak to zrobi\u0107: VBA natywnie nie obs\u0142uguje parsowania ani generowania\
  \ JSON, wi\u0119c u\u017Cyjemy j\u0119zyka skryptowego, takiego jak JScript (za\
  \ po\u015Brednictwem obiektu\u2026"
lastmod: '2024-03-13T22:44:35.255381-06:00'
model: gpt-4-0125-preview
summary: "VBA natywnie nie obs\u0142uguje parsowania ani generowania JSON, wi\u0119\
  c u\u017Cyjemy j\u0119zyka skryptowego, takiego jak JScript (za po\u015Brednictwem\
  \ obiektu ScriptControl), do parsowania ci\u0105g\xF3w JSON i budowania obiekt\xF3\
  w JSON."
title: Praca z JSON
weight: 38
---

## Jak to zrobić:
VBA natywnie nie obsługuje parsowania ani generowania JSON, więc użyjemy języka skryptowego, takiego jak JScript (za pośrednictwem obiektu ScriptControl), do parsowania ciągów JSON i budowania obiektów JSON. Oto jak możesz zanalizować ciąg JSON w VBA:

```basic
Function ParseJSON(ByVal jsonString As String) As Object
    Dim scriptControl As Object
    Set scriptControl = CreateObject("MSScriptControl.ScriptControl")
    scriptControl.Language = "JScript"
    
    scriptControl.Eval "var obj = (" & jsonString & ")"
    Set ParseJSON = scriptControl.CodeObject.obj
End Function

Sub DemoParseJSON()
    Dim jsonString As String
    jsonString = "{""name"":""John"", ""age"":30, ""city"":""New York""}"
    
    Dim parsed As Object
    Set parsed = ParseJSON(jsonString)
    
    MsgBox "Name: " & parsed.name & ", Age: " & parsed.age & ", City: " & parsed.city
End Sub
```

Aby wygenerować JSON, można użyć podobnego podejścia, budując ciąg JSON przez konkatenację:

```basic
Function GenerateJSON(name As String, age As Integer, city As String) As String
    GenerateJSON = "{""name"":""" & name & """, ""age"":" & age & ", ""city"":""" & city & """}"
End Function

Sub DemoGenerateJSON()
    Dim jsonString As String
    jsonString = GenerateJSON("Jane", 28, "Los Angeles")
    
    MsgBox jsonString
End Sub
```

## Szczegółowa analiza
Przedstawione podejścia korzystają z kontrolki ScriptControl do obsługi JSON, zasadniczo zlecając pracę silnikowi JavaScript. Jest to kreatywne obejście, ale niekoniecznie najbardziej wydajny czy nowoczesny sposób pracy z JSON w kontekście VBA. W bardziej skomplikowanych aplikacjach ta metoda może stać się uciążliwa i wprowadzić nadmiarowy narzut wydajnościowy lub obawy dotyczące bezpieczeństwa, ponieważ ScriptControl wykonuje się w środowisku, które ma pełny dostęp do komputera hosta.

Inne środowiska programistyczne, takie jak Python czy JavaScript, oferują wbudowane wsparcie dla JSON, czyniąc je bardziej odpowiednimi dla aplikacji wymagających obszernej manipulacji danymi JSON. Te języki zapewniają kompleksowe biblioteki, które ułatwiają nie tylko parsowanie i generowanie, ale także zapytania i formatowanie danych JSON.

Pomimo tych ograniczeń w VBA, zrozumienie, jak pracować z JSON, jest niezbędne w świecie, gdzie wymiana danych oparta na sieci Web i pliki konfiguracyjne są przeważnie formatowane w JSON. Dla programistów VBA opanowanie tych technik otwiera możliwości integracji z interfejsami API sieci Web, interpretacji plików konfiguracyjnych, a nawet budowy prostych aplikacji internetowych. Jednak, gdy projekty rosną w złożoności lub wymagają wysokiej wydajności, deweloperzy mogą rozważyć korzystanie z bardziej przyjaznych dla JSON środowisk programistycznych.
