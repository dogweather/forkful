---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:00:04.830060-07:00
description: "Refaktorering i programmering inneb\xE6rer \xE5 endre strukturen p\xE5\
  \ koden uten \xE5 endre dens oppf\xF8rsel, for \xE5 forbedre aspekter som lesbarhet,\
  \ vedlikeholdbarhet\u2026"
lastmod: '2024-03-13T22:44:40.629769-06:00'
model: gpt-4-0125-preview
summary: "Refaktorering i programmering inneb\xE6rer \xE5 endre strukturen p\xE5 koden\
  \ uten \xE5 endre dens oppf\xF8rsel, for \xE5 forbedre aspekter som lesbarhet, vedlikeholdbarhet\
  \ eller ytelse."
title: Refaktorering
weight: 19
---

## Hvordan:
Ta i betraktning et grunnleggende eksempel i Visual Basic for Applications (VBA) hvor vi har en subrutine som skriver ut detaljer om en ansatt. Opprinnelig er koden rotete, utfordrende å vedlikeholde eller utvide.

```vb
Sub PrintEmployeeDetails()
    Dim name As String
    Dim age As Integer
    Dim department As String
    name = "John Doe"
    age = 30
    department = "IT"
    
    MsgBox "Navn: " & name & vbCrLf & "Alder: " & age & vbCrLf & "Avdeling: " & department
End Sub
```

Refaktoreringstrinn 1: Trekk ut metode. En av de mest vanlige refaktoreringsteknikkene er å ta en spesifikk del av koden og flytte den inn i sin egen metode. Dette gjør koden mer modulær og lettere å forstå.

```vb
Sub PrintEmployeeDetails()
    Dim name As String
    Dim age As Integer
    Dim department As String
    name = "John Doe"
    age = 30
    department = "IT"
    
    VisMelding name, age, department
End Sub

Private Sub VisMelding(name As String, age As Integer, department As String)
    MsgBox "Navn: " & name & vbCrLf & "Alder: " & age & vbCrLf & "Avdeling: " & department
End Sub
```

Refaktoreringstrinn 2: Bruk en struktur. Dette trinnet innebærer å bruke en datastruktur for å holde relaterte data, noe som forbedrer kodeklarheten og gjør det enklere å overføre grupperte data.

```vb
Type Ansatt
    name As String
    age As Integer
    department As String
End Type

Sub PrintEmployeeDetails()
    Dim emp As Ansatt
    emp.name = "John Doe"
    emp.age = 30
    emp.department = "IT"
    
    VisMelding emp
End Sub

Private Sub VisMelding(emp As Ansatt)
    MsgBox "Navn: " & emp.name & vbCrLf & "Alder: " & emp.age & vbCrLf & "Avdeling: " & emp.department
End Sub
```

Disse trinnene omdanner rotet kode til modulær, strukturert kode, noe som betydelig forbedrer lesbarheten og vedlikeholdbarheten.

## Dybde
Konseptet med refaktorering er like gammelt som programmering selv, men det var Martin Fowlers bok "Refaktorering: Å forbedre designet av eksisterende kode" som brakte det inn i hovedstrømmen, og understreket dets viktighet i programvareutviklingsprosessen. I Visual Basic for Applications kan refaktorering være noe mer utfordrende på grunn av mangelen på innebygde verktøy som finnes i mer moderne integrerte utviklingsmiljøer (IDEer) som støtter automatisert refaktorering.

Dette reduserer imidlertid ikke dens viktighet. Selv i VBA kan anvendelsen av grunnleggende refaktoreringsteknikker manuelt, betydelig forbedre kodebasen, gjøre den renere og mer effektiv. Selv om VBA kanskje ikke har de samme moderne bekvemmelighetene, forblir prinsippene for god kodeutforming universelle. Utviklere som kommer fra andre språk kan finne den manuelle prosessen kjedelig, men vil utvilsomt sette pris på fordelene med å investere tid i å forbedre kodekvaliteten fra starten av.

For mer robuste utviklingsmiljøer eller når man jobber med spesielt sofistikerte prosjekter, kan det være verdt å utforske alternativer som tilbyr mer kraftfulle refaktoreringverktøyer eller konvertere VBA-prosjekter til et .NET-språk hvor Visual Studio tilbyr omfattende støtte for refaktorering. Likevel er forståelsen og anvendelsen av refaktoreringprinsipper i VBA en verdifull ferdighet som understreker viktigheten av å skrive ren, vedlikeholdbar kode, uansett miljøet.
