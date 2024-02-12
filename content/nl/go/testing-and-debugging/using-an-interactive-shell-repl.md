---
title:                "Gebruik van een interactieve shell (REPL)"
aliases:
- nl/go/using-an-interactive-shell-repl.md
date:                  2024-02-03T18:10:18.316817-07:00
model:                 gpt-4-0125-preview
simple_title:         "Gebruik van een interactieve shell (REPL)"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/go/using-an-interactive-shell-repl.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Een interactieve shell, of Lees-Evalueer-Print-Lus (REPL), stelt je in staat om in real-time met Go-code te experimenteren, commando's uit te voeren en onmiddellijke feedback te krijgen. Deze benadering wordt veel gebruikt voor leren, debuggen en prototypen, omdat het de traditionele cyclus van bewerken-compileren-uitvoeren omzeilt, waardoor het ontwikkelingsproces sneller en intuïtiever wordt.

## Hoe te:

Hoewel Go geen ingebouwde REPL bevat, heeft de gemeenschap tools zoals `gore` gecreëerd om dit gat te vullen. Installeer eerst `gore` door te draaien:

```
$ go get -u github.com/motemen/gore
```

Eenmaal geïnstalleerd, start je `gore` door `gore` te typen in je terminal:

```
$ gore
```

Je zou een prompt moeten zien die klaar is om Go-commando's te accepteren. Laten we een eenvoudig voorbeeld proberen:

```
gore> :import fmt
gore> fmt.Println("Hallo, Go REPL!")
```

Je zou een uitvoer zoals deze moeten zien:

```
Hallo, Go REPL!
```

Variabelen en functiedefinities werken zoals verwacht. Je kunt een functie declareren:

```
gore> :import math
gore> areaCircle := func(radius float64) float64 {
...> return math.Pi * radius * radius
...> }
gore> fmt.Println("Oppervlakte van cirkel met straal 4:", areaCircle(4))
```

En krijg meteen de uitvoer:

```
Oppervlakte van cirkel met straal 4: 50.26548245743669
```

## Diepere Duik:

Het concept van een REPL is oeroud, terug te voeren tot de Lisp machines van de jaren 1960, en biedt een interactieve programmeerervaring. In tegenstelling tot talen zoals Python of JavaScript, is Go ontworpen zonder een REPL, met de focus in plaats daarvan op gecompileerde binaries voor prestatie en eenvoud. Dit weerspiegelt de filosofie van Go van eenvoud en zijn ontwerp voor schaalbare en onderhoudbare software.

Echter, tools zoals `gore` of `goplay` tonen de vindingrijkheid van de Go-gemeenschap in het overbruggen van deze kloof. Deze tools parseren dynamisch Go-code en gebruiken het `go/eval` pakket of vergelijkbare mechanismen om het in real-time uit te voeren, zij het met enkele beperkingen vergeleken met een native REPL-omgeving. Deze beperkingen komen voort uit het type systeem en compilatiemodel van Go, wat on-the-fly evaluatie uitdagend kan maken.

Hoewel REPL-omgevingen buitengewoon nuttig zijn voor onderwijs en snelle tests, neigt het Go-ecosysteem typisch naar de traditionele compileer-en-run-processen voor de meeste ontwikkelingstaken. IDEs en editors met Go-ondersteuning, zoals Visual Studio Code of GoLand, bieden geïntegreerde tools voor testen en debuggen die veel van de behoefte aan een REPL voor professionele ontwikkeling verlichten.

Voor verkennend programmeren, prototypen of leren, bieden REPLs zoals `gore` echter een waardevol alternatief, waardoor programmeurs die gewend zijn aan REPLs in andere talen een soortgelijke ervaring in Go kunnen genieten.
