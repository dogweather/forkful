---
title:                "Fouten afhandelen"
aliases:
- /nl/vba/handling-errors/
date:                  2024-02-01T21:55:20.886291-07:00
model:                 gpt-4-0125-preview
simple_title:         "Fouten afhandelen"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/vba/handling-errors.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Foutafhandeling in Visual Basic for Applications (VBA) verwijst naar het proces van anticiperen, detecteren en oplossen van programmeer-, applicatie- of communicatiefouten. Het implementeren van robuuste foutafhandeling is cruciaal voor het behouden van de integriteit van applicaties en het verbeteren van de gebruikerservaring door onverwachte problemen elegant te beheren zonder abrupte crashes of dataverlies te veroorzaken.

## Hoe:

In VBA wordt foutafhandeling typisch geïmplementeerd met behulp van de `On Error`-instructie die VBA vertelt hoe verder te gaan wanneer er een fout optreedt. De meest voorkomende strategieën voor foutafhandeling betreffen de `On Error GoTo`-label, `On Error Resume Next` en `On Error GoTo 0`.

**Voorbeeld 1: Gebruik van `On Error GoTo`**

Deze benadering stelt je in staat om het programma naar een specifiek gedeelte van de code te sturen, gelabeld onmiddellijk na het tegenkomen van een fout.

```vb
Sub ErrorHandlerExample()
    On Error GoTo ErrHandler
    Dim intDivision As Integer

    intDivision = 5 / 0 ' Dit zal een fout door delen door nul veroorzaken

    Exit Sub
ErrHandler:
    MsgBox "Er is een fout opgetreden: " & Err.Description, vbCritical, "Fout!"
    Resume Next
End Sub
```

In dit voorbeeld zal elke runtime-fout de sprong naar `ErrHandler` activeren, een foutmelding weergeven en vervolgens doorgaan met de volgende regel na de fout.

**Voorbeeld 2: Gebruik van `On Error Resume Next`**

Deze strategie instrueert VBA om de volgende regel code uit te voeren, zelfs als er een fout optreedt, wat handig kan zijn voor fouten die naar verwachting onschadelijk zijn of als je van plan bent de fout later in de uitvoering te behandelen.

```vb
Sub ResumeNextExample()
    On Error Resume Next
    Dim intDivision As Integer
    intDivision = 5 / 0 ' Dit zal het programma niet stoppen; fout wordt genegeerd
    
    ' Controleer of er een fout opgetreden is
    If Err.Number <> 0 Then
        MsgBox "Er is een fout opgetreden: " & Err.Description, vbExclamation, "Afgehandelde Fout"
        ' Fout resetten
        Err.Clear
    End If
End Sub
```

In dit geval stopt het programma niet bij een fout; het controleert of er een fout is opgetreden, handelt deze af als dat zo is en wist vervolgens de fout.

## Diepere Duik

Historisch gezien is foutafhandeling in programmeertalen geëvolueerd van simpele goto-statements naar meer geavanceerde mechanismen zoals uitzonderingen in talen zoals Java en C#. De foutafhandeling van VBA, hoewel niet zo krachtig of flexibel als moderne uitzonderingsafhandeling, dient zijn doel binnen de context van de toepassing van de taal bij het automatiseren van taken in Microsoft Office-omgevingen.

De primaire beperking van VBA's foutafhandeling ligt in zijn enigszins omslachtige en handmatige benadering, waarbij zorgvuldige plaatsing van foutafhandelingscode en duidelijk begrip van de uitvoeringsstroom vereist zijn. Moderne programmeertalen bieden meestal meer elegante oplossingen, zoals try-catch-blokken, die automatisch de stroom naar foutafhandelingscode beheren zonder de noodzaak voor handmatige controles of sprongen in code-uitvoering.

Ondanks deze beperkingen, zijn de foutafhandelingsmechanismen van VBA geschikt voor de meeste automatiseringstaken en kunnen, indien correct gebruikt, de kans op onbehandelde fouten die problemen voor gebruikers veroorzaken aanzienlijk verminderen. Daarnaast kan het begrijpen van VBA's foutafhandeling inzicht bieden in oudere programmeerparadigma's en de evolutie van foutafhandelingsstrategieën in softwareontwikkeling.
