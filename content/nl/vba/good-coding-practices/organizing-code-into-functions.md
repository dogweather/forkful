---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:57:03.344277-07:00
description: "Het organiseren van code in functies in Visual Basic for Applications\
  \ (VBA) houdt in dat een programma opgedeeld wordt in kleinere, beheersbare stukjes,\u2026"
lastmod: '2024-03-11T00:14:24.462159-06:00'
model: gpt-4-0125-preview
summary: "Het organiseren van code in functies in Visual Basic for Applications (VBA)\
  \ houdt in dat een programma opgedeeld wordt in kleinere, beheersbare stukjes,\u2026"
title: Code organiseren in functies
---

{{< edit_this_page >}}

## Wat & Waarom?

Het organiseren van code in functies in Visual Basic for Applications (VBA) houdt in dat een programma opgedeeld wordt in kleinere, beheersbare stukjes, bekend als functies. Programmeurs doen dit om de leesbaarheid van de code te verbeteren, code efficiënt te hergebruiken en het debuggen en onderhoudsprocessen te vereenvoudigen.

## Hoe:

In VBA worden functies gedefinieerd met de `Function` en `End Function` instructies. Hier is een eenvoudig voorbeeld van hoe je een functie creëert die het oppervlakte van een rechthoek berekent:

```basic
Function CalculateArea(lengte As Double, breedte As Double) As Double
    CalculateArea = lengte * breedte
End Function
```

Om deze functie in je VBA-code aan te roepen en het resultaat in een berichtvenster weer te geven, zou je gebruiken:

```basic
Sub ShowArea()
    Dim oppervlakte As Double
    oppervlakte = CalculateArea(10, 5)
    MsgBox "De oppervlakte is " & oppervlakte
End Sub
```

Wanneer uitgevoerd, toont deze code een berichtvenster dat zegt: `De oppervlakte is 50`.

### Variabelen Doorsturen ByRef en ByVal

VBA staat je toe om variabelen naar functies door te sturen ofwel bij referentie (`ByRef`) ofwel bij waarde (`ByVal`). Het eerste betekent dat de originele variabele gewijzigd kan worden door de functie, terwijl het laatste een kopie doorstuurt, de originele variabele beschermend tegen veranderingen.

```basic
Function ModifyValue(ByRef num As Integer)
    num = num + 5
End Function

Function PreserveValue(ByVal num As Integer) As Integer
    num = num + 5
    PreserveValue = num
End Function
```

## Diepere Duik

VBA, als een event-gedreven programmeertaal, legt aanzienlijke nadruk op functies en subroutines om diverse taken te behandelen. In tegenstelling tot veel moderne talen, heeft VBA een unieke eigenschap waar het `Function` sleutelwoord niet alleen een blok herbruikbare code verklaart, maar ook een impliciete retourwaarde toelaat die direct toegewezen wordt aan de naam van de functie.

Historisch gezien is het ontwerp van VBA-functies beïnvloed door eerdere programmeerparadigma's waar inkapseling en modulariteit geleidelijk werden erkend om hun belang in softwareontwikkeling. Deze historische achtergrond heeft VBA ertoe geleid een enigszins behoudende maar functionele benadering te adopteren voor het organiseren van code.

Hoewel VBA krachtig is binnen zijn native omgevingen (bijv. Microsoft Office-toepassingen), is het essentieel op te merken dat de programmeerwereld geëvolueerd is. Talen zoals Python bieden een eenvoudiger syntax en een uitgebreide standaardbibliotheek, waardoor ze een gunstiger alternatief vormen voor verschillende toepassingen buiten de Office-suite. Echter, bij het werken binnen Microsoft Office-producten zijn de integratie- en automatiseringsmogelijkheden die VBA biedt ongeëvenaard.

Het is vermeldenswaard dat ondanks zijn leeftijd, de gemeenschap rondom VBA actief blijft en voortdurend innovatieve manieren vindt om zijn functionaliteit te benutten. Toch, terwijl de software-industrie zich richt op modernere, veelzijdigere en robuustere talen, worden programmeurs die bekend zijn met VBA aangemoedigd deze alternatieven te verkennen voor niet-Office gerelateerde taken om hun coderingsgereedschapskist te verbreden.
