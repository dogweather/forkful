---
title:                "Een debugger gebruiken"
aliases: - /nl/vba/using-a-debugger.md
date:                  2024-02-01T22:04:24.294701-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een debugger gebruiken"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/vba/using-a-debugger.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Een debugger gebruiken in Visual Basic for Applications (VBA) houdt in dat je je code stap voor stap uitvoert om de uitvoeringsstroom en de status van variabelen te inspecteren. Dit proces is cruciaal voor het identificeren en herstellen van fouten in je code, uiteindelijk om te zorgen dat deze presteert zoals verwacht.

## Hoe te:

In VBA is de debugger een integraal onderdeel van de Visual Basic Editor (VBE). Zo kun je er gebruik van maken:

1. **Breekpunten instellen**: Klik in de linkermarge naast de code regel waarin je geïnteresseerd bent, of plaats je cursor op de regel en druk op F9. Dit vertelt VBA om de uitvoering te pauzeren wanneer het dit punt bereikt.

    ```vb
    Sub DebugVoorbeeld()
        Dim teller Als Integer
        Voor teller = 1 Tot 5
            Debug.Print teller ' Stel breekpunt hier in
        Volgende teller
    End Sub
    ```

    Wanneer de code wordt uitgevoerd, zal het pauzeren op de `Debug.Print teller` regel, waardoor je variabele waarden kunt inspecteren.

2. **Stap In (F8)**: Met dit commando voer je je code uit, één verklaring per keer, waarbij je elke aangeroepen procedure binnengaat. Het is nuttig voor het traceren van hoe je code en functies interageren.

3. **Watch Venster**: Gebruik het Watch Venster om de waarden van variabelen of uitdrukkingen te monitoren. Als een variabele niet binnen bereik is, zal het Watch Venster dit aangeven. Klik met de rechtermuisknop op een variabele > Voeg Watch toe.

4. **Immediate Window (Ctrl+G)**: Dit venster is vooral nuttig voor het testen van uitdrukkingen of het wijzigen van variabele waarden tijdens het debuggen. Typ `?variabeleNaam` om de huidige waarde van een variabele te printen, of wijs een nieuwe waarde toe met `variabeleNaam = nieuweWaarde`.

    ```vb
    ' In Immediate Window
    ?teller ' Print de huidige waarde van teller
    teller = 3 ' Zet de waarde van teller op 3
    ```

5. **Voorbeelduitvoer**:

    Wanneer je het breekpunt bereikt en regel voor regel uitvoert met F8, zou het Immediate Window zoiets kunnen weergeven als:

    ```
    teller = 1
    teller = 2
    teller = 3
    ```

    Hier hebben we handmatig de `teller` variabele opgevraagd na elke iteratie.

## Diepgaande duik:

De debugger in VBA, hoewel robuust, maakt deel uit van een bredere traditie van debugtools in programmeertalen, aanzienlijk geëvolueerd vanaf zijn vroege voorgangers. Geïntroduceerd met de eerste versies van VBA, was het bedoeld om ontwikkelaars te voorzien van een eenvoudige maar krachtige set van tools voor code-inspectie en -correctie. In de loop van de tijd hebben verbeteringen voorwaardelijke breekpunten, verbeterde watch mogelijkheden en integratie met de Excel-interface voor een intuïtievere data-inspectie omvat.

Echter, vergeleken met moderne Integrated Development Environments (IDE's) zoals Visual Studio of Eclipse, kunnen de debugtools van VBA basisch lijken. Deze moderne IDE's bieden geavanceerdere functies zoals real-time variabele inspectie, geavanceerde breekpunten en geïntegreerde unit testing frameworks. Hoewel deze alternatieven een meer omvattende debugervaring bieden, blijven de eenvoud en directheid van VBA's debugger goed geschikt voor de specifieke context van automatisering en scripting binnen Microsoft Office-applicaties.

Voor programmeurs die gewend zijn aan deze moderne omgevingen, kan aanpassen aan de debugtools van VBA een verschuiving in benadering vereisen. Toch zijn de fundamentele principes van het inspecteren van variabelen, het doorlopen van code, en het observeren van runtime gedrag universeel. Met oefening wordt VBA's debugger een onmisbaar hulpmiddel voor het zorgen dat je automatiseringsscripts foutloos presteren binnen het Office-ecosysteem.
