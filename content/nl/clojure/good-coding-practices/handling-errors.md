---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:01:54.897664-07:00
description: "Foutafhandeling gaat over het beheren van het onverwachte in programma's\
  \ - zoals een uitsmijter die te maken heeft met lastpakken. Programmeurs houden\
  \ van\u2026"
lastmod: '2024-03-13T22:44:50.427665-06:00'
model: gpt-4-0125-preview
summary: Foutafhandeling gaat over het beheren van het onverwachte in programma's
  - zoals een uitsmijter die te maken heeft met lastpakken.
title: Fouten afhandelen
weight: 16
---

## Wat & Waarom?
Foutafhandeling gaat over het beheren van het onverwachte in programma's - zoals een uitsmijter die te maken heeft met lastpakken. Programmeurs houden van een soepele afhandeling; foutafhandeling helpt om de problemen in toom te houden, zodat hun code niet struikelt en valt wanneer ze met het onverwachte worden geconfronteerd.

## Hoe te:
Clojure, net als zijn Lisp-voorouders, leunt op uitzonderingen voor het omgaan met fouten. Hier zie je wat je in je mars hebt als de zaken mis gaan.

Een uitzondering gooien is eenvoudig:
```Clojure
(throw (Exception. "Oeps! Er ging iets mis."))
```

Een uitzondering vangen, dit ga je veel doen:
```Clojure
(try
  ;; risicovolle code
  (/ 1 0)
  (catch ArithmeticException e
    (println "Kan niet delen door nul!"))
  ;; finally blok wordt hoe dan ook uitgevoerd
  (finally 
    (println "Code voor opruimen gaat hier.")))
```
Voorbeelduitvoer voor het catch-blok hierboven:
```
Kan niet delen door nul!
Code voor opruimen gaat hier.
```

`ex-info` en `ex-data` gebruiken voor rijkere context over uitzonderingen:
```Clojure
(try
  ;; een aangepaste uitzondering veroorzaken
  (throw (ex-info "Aangepaste fout" {:type :aangepast-falen}))
  (catch Exception e
    ;; de gegevens uit onze aangepaste uitzondering halen
    (println (ex-data e))))
```
Voorbeelduitvoer:
```
{:type :aangepast-falen}
```

## Diepere Duik
Het verhaal van foutafhandeling in Clojure is niet radicaal anders dan bij andere Lisps of zelfs Java (waarvan het het `try-catch` mechanisme erft). Het is pragmatisch; uitzonderingen gebruiken is de hoofdroute, net zoals bij Java, maar Clojure biedt een functionele smaak met `ex-info` en `ex-data` voor rijker foutgegevens.

Alternatieven voor foutafhandeling in Clojure omvatten het gebruik van monadische constructies, zoals de `either` monad uit bibliotheken zoals `cats`, of core.async voor kanaalgebaseerde foutverspreiding. Echter, deze zijn complexer en worden in specifieke scenario's gebruikt.

Historisch gezien heeft foutafhandeling in programmeertalen zich ontwikkeld van eenvoudige statusretouren naar de meer geavanceerde uitzonderingsafhandelingsmechanismen van moderne talen. Clojure kiest voor eenvoud met een vleugje functioneel programmeren, en mengt het oude met het nieuwe.

## Zie Ook
- Clojure's handleiding voor uitzonderingen: https://clojure.org/guides/exceptions
- “Cats” bibliotheek voor meer functionele benaderingen: https://github.com/funcool/cats
- “Core.async” voor asynchroon programmeren: https://github.com/clojure/core.async
