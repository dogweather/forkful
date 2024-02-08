---
title:                "Lavorare con XML"
aliases:
- it/vba/working-with-xml.md
date:                  2024-02-01T22:06:48.241036-07:00
model:                 gpt-4-0125-preview
simple_title:         "Lavorare con XML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/vba/working-with-xml.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa e perché?

Lavorare con XML in Visual Basic for Applications (VBA) comporta l'analisi, la creazione e la modifica di documenti XML nel contesto delle applicazioni Microsoft Office. I programmatori si rivolgono a questa capacità per integrare le applicazioni Office con i servizi web o altre fonti di dati che emettono XML, facilitando lo scambio di dati e le funzionalità di reporting.

## Come fare:

Per iniziare a interagire con XML, si utilizza solitamente l'oggetto `MSXML2.DOMDocument`. Questa interfaccia ti consente di caricare, analizzare e navigare i documenti XML. Di seguito è riportato un semplice esempio che dimostra come caricare un file XML, navigare nella sua struttura e leggere attributi e contenuti di testo.

```basic
' Prima, assicurati di aver aggiunto il riferimento a "Microsoft XML, v6.0" tramite Strumenti -> Riferimenti
Dim xmlDoc As MSXML2.DOMDocument60
Set xmlDoc = New MSXML2.DOMDocument60
xmlDoc.async = False
xmlDoc.Load("C:\Percorso\Al\Tuo\File.xml") ' Carica il tuo file XML

' Controlla se l'XML è stato caricato con successo
If xmlDoc.parseError.ErrorCode <> 0 Then
    MsgBox "Errore nel caricamento XML:" & xmlDoc.parseError.reason
Else
    ' Naviga e leggi gli elementi
    Dim book As IXMLDOMNode
    Set book = xmlDoc.SelectSingleNode("//book/title") ' XPath per trovare il primo <title> all'interno di <book>
    MsgBox book.Text ' Mostra il testo del titolo
End If
```

Nel codice di esempio sopra, creiamo un'istanza di `MSXML2.DOMDocument60`, caricando un file XML e poi controlliamo gli errori. Se non vengono trovati errori, navighiamo verso un nodo specifico usando XPath e visualizziamo il suo contenuto di testo.

## Approfondimento:

L'integrazione delle capacità XML in VBA risale ai primi anni 2000, quando è cresciuta la necessità per le applicazioni Office di interagire con i dati e i servizi web. La libreria `MSXML`, o Microsoft XML Core Services, si è evoluta nel corso degli anni, con `MSXML2.DOMDocument60` che è una delle versioni più recenti consigliate per l'uso a causa del suo miglioramento in termini di prestazioni e caratteristiche di sicurezza.

Sebbene potente, le capacità di gestione XML di VBA sono considerate meno efficienti e più ingombranti rispetto agli ambienti di programmazione moderni come XML.etree di Python o LINQ to XML di C#. La verbosità intrinseca di VBA e la necessità di aggiungere e gestire manualmente i riferimenti possono ostacolare uno sviluppo rapido. Inoltre, con l'avvento di JSON come formato di interscambio dati più leggero, molti programmatori e applicazioni si stanno allontanando dall'XML a meno che non sia necessaria l'interoperabilità con sistemi legacy o servizi aziendali specifici.

Tuttavia, per compiti che richiedono l'analisi o la generazione di documenti XML nel contesto dell'automazione di Microsoft Office, sfruttare le caratteristiche di gestione XML di VBA rimane un approccio praticabile e talvolta necessario. Ciò permette di trovare un equilibrio tra l'accesso al ricco set di funzionalità delle applicazioni Office e le capacità di manipolazione dei dati strutturati fornite da XML.
