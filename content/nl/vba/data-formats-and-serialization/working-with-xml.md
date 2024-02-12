---
title:                "Werken met XML"
date:                  2024-02-01T22:06:47.430484-07:00
model:                 gpt-4-0125-preview
simple_title:         "Werken met XML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/vba/working-with-xml.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Werken met XML in Visual Basic for Applications (VBA) houdt in het parsen, creëren en wijzigen van XML-documenten binnen de context van Microsoft Office-applicaties. Programmeurs wenden zich tot deze mogelijkheid voor de integratie van Office-applicaties met webservices of andere gegevensbronnen die XML uitsturen, wat de uitwisseling van gegevens en rapportagefunctionaliteiten vergemakkelijkt.

## Hoe:

Om te beginnen met het interageren met XML, gebruikt men meestal het `MSXML2.DOMDocument` object. Deze interface stelt u in staat om XML-documenten te laden, te parsen en te navigeren. Hieronder staat een simpel voorbeeld dat demonstreert hoe je een XML-bestand laadt, door de structuur navigeert en attributen en tekstinhoud leest.

```basic
' Zorg er eerst voor dat je de referentie naar "Microsoft XML, v6.0" hebt toegevoegd via Extra -> Referenties
Dim xmlDoc As MSXML2.DOMDocument60
Set xmlDoc = New MSXML2.DOMDocument60
xmlDoc.async = False
xmlDoc.Load("C:\Pad\Naar\Jouw\Bestand.xml") ' Laad je XML-bestand

' Controleer of het XML succesvol geladen is
If xmlDoc.parseError.ErrorCode <> 0 Then
    MsgBox "Fout bij het laden van XML:" & xmlDoc.parseError.reason
Else
    ' Navigeer en lees elementen
    Dim book As IXMLDOMNode
    Set book = xmlDoc.SelectSingleNode("//book/title") ' XPath om de eerste <title> binnen <book> te vinden
    MsgBox book.Text ' Toon de titeltekst
End If
```

In de bovenstaande voorbeeldcode creëren we een instantie van `MSXML2.DOMDocument60`, laden we een XML-bestand en controleren we op fouten. Als er geen fouten gevonden zijn, navigeren we naar een specifieke node met behulp van XPath en tonen we de tekstinhoud ervan.

## Diepere Duik:

De integratie van XML-mogelijkheden in VBA gaat terug tot de vroege jaren 2000, toen de behoefte begon te groeien voor Office-applicaties om te interageren met webgegevens en -services. De `MSXML`-bibliotheek, of Microsoft XML Core Services, is in de loop der jaren geëvolueerd, met `MSXML2.DOMDocument60` als een van de laatste aanbevolen versies voor gebruik vanwege de verbeterde prestaties en veiligheidskenmerken.

Hoewel krachtig, worden de XML-behandelingsmogelijkheden van VBA als minder efficiënt en omslachtiger beschouwd in vergelijking met moderne programmeeromgevingen zoals Python's XML.etree of C#'s LINQ to XML. De inherente langdradigheid van VBA en de vereiste om handmatig referenties toe te voegen en te beheren, kunnen snelle ontwikkeling afschrikken. Verder, met de komst van JSON als een lichter data-uitwisselingsformaat, verschuiven veel programmeurs en applicaties weg van XML, tenzij interoperabiliteit met oudere systemen of specifieke bedrijfsservices het gebruik ervan noodzakelijk maakt.

Echter, voor taken die het parsen of genereren van XML-documenten binnen de context van Microsoft Office-automatisering vereisen, blijft het benutten van VBA's XML-behandelingsfuncties een haalbare en soms noodzakelijke benadering. Dit vindt een evenwicht tussen toegang tot de rijke functionaliteit van Office-applicaties en de gestructureerde gegevensmanipulatiemogelijkheden die XML biedt.
