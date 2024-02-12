---
title:                "Arbeide med komplekse tall"
aliases:
- /no/vba/working-with-complex-numbers/
date:                  2024-02-01T22:07:42.718758-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arbeide med komplekse tall"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/vba/working-with-complex-numbers.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Arbeid med komplekse tall innebærer å utføre matematiske operasjoner på tall som har både en reell del og en imaginær del. Programmerere tar ofte for seg komplekse tall i domener som ingeniørvitenskap, fysikk og overalt hvor det er nødvendig å løse ligninger som ikke er mulig med bare reelle tall.

## Hvordan:

I Visual Basic for Applications (VBA), kan håndtering av komplekse tall være noe mindre rett frem sammenlignet med språk med innebygget støtte for dem. Du kan imidlertid håndtere komplekse operasjoner ved å opprette funksjoner eller bruke eksisterende bibliotekfunksjoner. La oss utforske et grunnleggende eksempel på addisjon, subtraksjon, multiplikasjon og divisjon av komplekse tall:

```vb
' Funksjon for å legge sammen komplekse tall
Function AddComplex(x As String, y As String) As String
    Dim real1 As Double, imag1 As Double
    Dim real2 As Double, imag2 As Double
    
    ' Trekker ut den reelle og imaginære delen fra de komplekse tallene
    real1 = Val(Split(x, "+")(0))
    imag1 = Val(Split(x, "+")(1))
    real2 = Val(Split(y, "+")(0))
    imag2 = Val(Split(y, "+")(1))
    
    ' Utfører addisjon
    AddComplex = (real1 + real2) & "+" & (imag1 + imag2) & "i"
End Function

' Eksempel på bruk
Sub ExampleUsage()
    Dim result As String
    result = AddComplex("3+2i", "1+7i")
    Debug.Print "Resultat av Addisjon: " & result  ' Output: Resultat av Addisjon: 4+9i
End Sub
```

Selv om dette demonstrerer addisjon, kan lignende tilnærminger tilpasses for subtraksjon, multiplikasjon og divisjon. For komplekse operasjoner utover grunnleggende aritmetikk, kan det være verdt å utforske eksterne biblioteker eller integrere andre løsninger som støtter operasjoner med komplekse tall mer innfødt.

## Dypdykk:

VBA inkluderer ikke innebygd støtte for komplekse tall, et aspekt der det henger etter språk som Python, som har en komplekstallklass (‘complex’) eller C++ med sitt Standard Template Library (‘std::complex’). Historisk sett er behovet for å manipulere komplekse tall direkte i VBA relativt sjeldent, da det ofte brukes til automatisering, manipulering av Office-applikasjoner, og oppgaver som tradisjonelt ikke krever komplekse matematiske beregninger. Da VBA ble oppfattet og utviklet, var dets bruksområder hovedsakelig fokusert på forretningsapplikasjoner snarere enn vitenskapelig databehandling, noe som kan forklare denne utelatelsen.

For oppgaver som krever omfattende manipulasjon av komplekse tall, kan programmerere finne det gunstig å bruke et mer matematisk orientert språk. Men, for de som er forpliktet til eller begrenset av bruken av VBA, er det å skrive egendefinerte funksjoner (som illustrert) eller integrere med programvare som har disse evnene (som MATLAB eller Excel selv i noen grad) levedyktige veier fremover. Til tross for sine begrensninger, kan kreative løsninger og eksterne integrasjoner utvide VBAs nytteverdi til domener det opprinnelig ikke var designet for, inkludert arbeid med komplekse tall.
