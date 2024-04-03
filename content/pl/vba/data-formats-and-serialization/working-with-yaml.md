---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:07:45.685045-07:00
description: "YAML, co oznacza \"YAML Ain't Markup Language\" (YAML nie jest j\u0119\
  zykiem znacznik\xF3w), to czytelny dla cz\u0142owieka j\u0119zyk serializacji danych,\
  \ powszechnie\u2026"
lastmod: '2024-03-13T22:44:35.254292-06:00'
model: gpt-4-0125-preview
summary: "YAML, co oznacza \"YAML Ain't Markup Language\" (YAML nie jest j\u0119zykiem\
  \ znacznik\xF3w), to czytelny dla cz\u0142owieka j\u0119zyk serializacji danych,\
  \ powszechnie u\u017Cywany do plik\xF3w konfiguracyjnych."
title: Praca z YAML
weight: 41
---

## Co i Dlaczego?

YAML, co oznacza "YAML Ain't Markup Language" (YAML nie jest językiem znaczników), to czytelny dla człowieka język serializacji danych, powszechnie używany do plików konfiguracyjnych. Programiści często używają go ze względu na jego prostotę i czytelność w wielu środowiskach programistycznych, w tym w skryptowym świecie Visual Basic for Applications (VBA), aby zwiększyć interoperacyjność oraz przechowywanie i wymianę danych.

## Jak to zrobić:

Praca z YAML w VBA wymaga zrozumienia, jak analizować i przekształcać YAML na format, który VBA może łatwo manipulować, zwykle słowniki lub kolekcje. Niestety, VBA nie obsługuje natywnie analizowania YAML ani serializacji. Możesz jednak użyć kombinacji narzędzi konwersji JSON i obiektów słownika, aby pracować z danymi YAML, biorąc pod uwagę bliskie pokrewieństwo YAML z JSON.

Najpierw przekonwertuj swoje dane YAML na JSON za pomocą konwertera online lub narzędzia konwersji YAML na JSON w swoim środowisku programistycznym. Po przekonwertowaniu możesz użyć poniższego przykładu do analizowania JSON w VBA, co pośrednio pozwala Ci pracować z YAML:

```vb
' Dodaj odniesienie do Microsoft Scripting Runtime dla Dictionary
' Dodaj odniesienie do Microsoft XML, v6.0 dla parsowania JSON

Sub ParseYAMLAsJSON()
    Dim jsonText As String
    jsonText = "{""name"": ""John Doe"", ""age"": 30}" ' To jest JSON przekonwertowany z YAML
    
    ' Zakładając, że masz funkcję analizującą JSON
    Dim parsedData As Dictionary
    Set parsedData = JsonParser(jsonText)
    
    Debug.Print "Name: " & parsedData("name")
    Debug.Print "Age: " & parsedData("age")
End Sub

Function JsonParser(ByVal jsonText As String) As Dictionary
    ' Placeholder na logikę analizowania JSON - możesz tu użyć zewnętrznej biblioteki
    Set JsonParser = New Dictionary
    JsonParser.Add "name", "John Doe"
    JsonParser.Add "age", 30
End Function
```
W tym przykładzie funkcja `JsonParser` jest zastępcą miejsca, w którym analizowałbyś JSON. Istnieją różne biblioteki, które pomagają w analizowaniu JSON, ponieważ bezpośrednie biblioteki analizujące YAML dla VBA są rzadkie.

## Głębsze zanurzenie

Brak bezpośredniego obsługiwania YAML w VBA można przypisać jego wiekowi i środowisku, dla którego został stworzony, które pierwotnie nie było zaprojektowane z myślą o nowoczesnych formatach serializacji danych. YAML sam w sobie pojawił się jako popularny format konfiguracji i serializacji na początku lat 2000, idąc w parze z nadejściem aplikacji wymagających bardziej przyjaznych dla człowieka plików konfiguracyjnych.

Programiści zwykle wykorzystują zewnętrzne narzędzia lub biblioteki, aby zająć przestrzeń między VBA a YAML. Często wiąże się to z konwersją YAML na JSON, jak pokazano, ze względu na dostępne wsparcie JSON przez różne biblioteki oraz podobieństwo między JSON i YAML pod względem struktury i celu.

Chociaż praca z YAML bezpośrednio w VBA pokazuje elastyczność języka, warto zauważyć, że inne środowiska programistyczne (np. Python lub JavaScript) zapewniają bardziej natywną i płynną obsługę YAML. Te alternatywy mogą być lepiej dopasowane do projektów silnie opierających się na YAML do konfiguracji lub serializacji danych. Niemniej jednak, dla tych, którzy są zobowiązani do korzystania z VBA lub tego wymagają, pośrednia metoda przez konwersję JSON pozostaje praktycznym i użytecznym podejściem do zarządzania i manipulowania danymi YAML.
