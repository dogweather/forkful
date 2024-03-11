---
date: 2024-01-20 18:03:04.201791-07:00
description: "Noen ganger trenger vi bare et blankt lerret. \xC5 starte et nytt prosjekt\
  \ i Clojure gir oss den freshe starten \u2013 for \xE5 utforske, eksperimentere,\
  \ eller bygge\u2026"
lastmod: '2024-03-11T00:14:13.925734-06:00'
model: gpt-4-1106-preview
summary: "Noen ganger trenger vi bare et blankt lerret. \xC5 starte et nytt prosjekt\
  \ i Clojure gir oss den freshe starten \u2013 for \xE5 utforske, eksperimentere,\
  \ eller bygge\u2026"
title: "\xC5 starte et nytt prosjekt"
---

{{< edit_this_page >}}

## What & Why?
Noen ganger trenger vi bare et blankt lerret. Å starte et nytt prosjekt i Clojure gir oss den freshe starten – for å utforske, eksperimentere, eller bygge noe nytt.

## How to:
For å kickstarte et nytt Clojure-prosjekt, får leiningen deg raskt i gang. Her er stegene:

```Clojure
; Installer Leiningen (https://leiningen.org/) først
; Åpne terminalen og kjør:
lein new app mitt-kule-prosjekt

; Naviger til prosjekt-mappen:
cd mitt-kule-prosjekt

; Start REPL for å eksperimentere:
lein repl
```

Etter disse kommandoene, ser du kanskje noe sånt:

```Clojure
Creating a new Clojure project in /.../mitt-kule-prosjekt.
nREPL server started on port 12345 on host 127.0.0.1 - nrepl://127.0.0.1:12345
mitt-kule-prosjekt.core=> 
```

Oppsettet er klart, og du er klar for å kode.

## Deep Dive:
Tilbake i 2009 ble Leiningen født for å gjøre Clojure-utvikling enklere. Alternativer finnes, slik som Boot eller direkte bruk av Clojure CLI-verktøy, men Leiningen holder fortsatt koken for de fleste prosjekter. Når prosjektet ditt vokser kan du legge til avhengigheter i `project.clj`, som er hjertet i Leiningen-prosjektets konfigurasjon.

## See Also:
- Leiningen: [https://leiningen.org/](https://leiningen.org/)
- Clojure Docs for å dykke dypere: [https://clojure.org/guides/getting_started](https://clojure.org/guides/getting_started)
- Boot, et byggverktøy alternativ: [http://boot-clj.com/](http://boot-clj.com/)
- Clojure CLI-verktøy doc: [https://clojure.org/guides/deps_and_cli](https://clojure.org/guides/deps_and_cli)

Nå er det bare å kode. Lykke til!
