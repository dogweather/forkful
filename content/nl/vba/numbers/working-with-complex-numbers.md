---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:08:05.450973-07:00
description: "Werken met complexe getallen houdt in dat je wiskundige bewerkingen\
  \ uitvoert op getallen die zowel een re\xEBel deel als een imaginair deel hebben.\u2026"
lastmod: '2024-02-25T18:49:47.974540-07:00'
model: gpt-4-0125-preview
summary: "Werken met complexe getallen houdt in dat je wiskundige bewerkingen uitvoert\
  \ op getallen die zowel een re\xEBel deel als een imaginair deel hebben.\u2026"
title: Werken met complexe getallen
---

{{< edit_this_page >}}

## Wat & Waarom?

Werken met complexe getallen houdt in dat je wiskundige bewerkingen uitvoert op getallen die zowel een reëel deel als een imaginair deel hebben. Programmeurs houden zich vaak bezig met complexe getallen in domeinen zoals engineering, natuurkunde en overal waar het oplossen van vergelijkingen, die niet mogelijk zijn met alleen reële getallen, vereist is.

## Hoe te:

In Visual Basic for Applications (VBA) kan het omgaan met complexe getallen enigszins minder rechttoe rechtaan zijn in vergelijking met talen die hier native ondersteuning voor bieden. Je kunt echter complexe bewerkingen beheren door functies te creëren of door bestaande bibliotheekfuncties te gebruiken. Laten we een basisvoorbeeld van optellen, aftrekken, vermenigvuldigen en delen van complexe getallen verkennen:

```vb
' Functie om complexe getallen op te tellen
Function AddComplex(x As String, y As String) As String
    Dim real1 As Double, imag1 As Double
    Dim real2 As Double, imag2 As Double
    
    ' Het extraheren van reële en imaginaire delen uit de complexe getallen
    real1 = Val(Split(x, "+")(0))
    imag1 = Val(Split(x, "+")(1))
    real2 = Val(Split(y, "+")(0))
    imag2 = Val(Split(y, "+")(1))
    
    ' De optelling uitvoeren
    AddComplex = (real1 + real2) & "+" & (imag1 + imag2) & "i"
End Function

' Voorbeeld van gebruik
Sub ExampleUsage()
    Dim resultaat As String
    resultaat = AddComplex("3+2i", "1+7i")
    Debug.Print "Resultaat van Optelling: " & resultaat  ' Uitvoer: Resultaat van Optelling: 4+9i
End Sub
```

Hoewel dit optellen demonstreert, kunnen vergelijkbare benaderingen worden aangepast voor aftrekken, vermenigvuldigen en delen. Voor complexe bewerkingen buiten de basisrekenkunde kan het de moeite waard zijn om externe bibliotheken te verkennen of andere oplossingen te integreren die complexe getalbewerkingen meer native ondersteunen.

## Diepere Duik:

VBA bevat geen ingebouwde ondersteuning voor complexe getallen, een aspect waarin het achterloopt op talen zoals Python, dat een complex getalklasse heeft (`complex`), of C++ met zijn Standard Template Library (`std::complex`). Historisch gezien is de behoefte om direct in VBA met complexe getallen te werken relatief zeldzaam, omdat het vaak wordt gebruikt voor automatisering, het manipuleren van Office-applicaties en taken die traditioneel geen complexe wiskundige berekeningen vereisen. Toen VBA werd bedacht en ontwikkeld, waren de use cases voornamelijk gericht op zakelijke toepassingen in plaats van wetenschappelijk rekenen, wat het weglating kan verklaren.

Voor taken die uitgebreide manipulaties van complexe getallen vereisen, kunnen programmeurs het nuttig vinden om een meer wiskundig georiënteerde taal te gebruiken. Echter, voor degenen die zich inzetten voor of beperkt zijn door het gebruik van VBA, zijn het schrijven van aangepaste functies (zoals geïllustreerd) of integratie met software die deze capaciteiten heeft (zoals MATLAB of Excel zelf tot op zekere hoogte) haalbare paden voorwaarts. Ondanks de beperkingen kunnen creatieve oplossingen en externe integraties de bruikbaarheid van VBA uitbreiden naar domeinen waarvoor het oorspronkelijk niet was ontworpen, inclusief het werken met complexe getallen.
