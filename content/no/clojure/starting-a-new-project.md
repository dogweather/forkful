---
title:                "Å starte et nytt prosjekt"
html_title:           "Clojure: Å starte et nytt prosjekt"
simple_title:         "Å starte et nytt prosjekt"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/starting-a-new-project.md"
---

{{< edit_this_page >}}

# Hvorfor

Å starte et nytt prosjekt kan være en spennende og givende opplevelse for utviklere. Med Clojure er det enda mer engasjerende, da språket gir mulighet for å bygge kraftige og skalerbare applikasjoner på en elegant og enkel måte.

# Hvordan

For å starte et nytt Clojure-prosjekt må du først installere Clojure på datamaskinen din. Dette kan gjøres ved å følge instruksjonene på [Clojure sin offisielle nettside] (https://clojure.org/guides/getting_started).

Når du har installert Clojure, kan du opprette en ny mappe for prosjektet ditt og initialisere det med [Leiningen] (https://leiningen.org/), et byggeverktøy for Clojure-prosjekter. Gå til mappen og skriv følgende kommando i terminalen:

```
lein new app min-app
```

Dette vil generere en standard mappesstruktur og noen filer for prosjektet ditt. Naviger til mappen for prosjektet og åpne filen "src/min_app/core.clj". Her kan du skrive din første Clojure-kode, for eksempel:

```
(defn greet [name]
  (str "Hei " name "!"))
```

For å kjøre denne funksjonen, skriv følgende i terminalen:

```
lein run -m min-app.core
```

Du bør se utskriften "Hei verden!" i terminalen din. Dette er bare en enkel demonstrasjon av hvordan du kan bruke Clojure for å bygge funksjonelle programmer.

# Dypdykk

Når du starter et nytt Clojure-prosjekt, kan det være nyttig å vite om noen best practices og verktøy som kan hjelpe deg underveis.

En viktig del av Clojure er dens felles standardbibliotek, eller "core library" som det kalles. Dette inneholder et stort utvalg av funksjoner og datastrukturer som du kan bruke til å bygge dine applikasjoner. Det er også mange tredjepartsbiblioteker tilgjengelig som kan utvide funksjonaliteten til Clojure.

En annen nyttig ting å vite er strukturen til Clojure-prosjekter. Standardmappestrukturen er generert av Leiningen, og den inkluderer mapper for kildekode, ressurser og tester. Det er også en "project.clj" fil som inneholder prosjektets avhengigheter og konfigurasjon.

For mer informasjon om hvordan du kan komme i gang med Clojure, kan du sjekke ut [Clojure sin offisielle dokumentasjon] (https://clojure.org/).

# Se også
- [Offisiell Clojure nettside] (https://clojure.org/)
- [Leiningen] (https://leiningen.org/)
- [Clojure byggeverktøy] (https://clojure.org/guides/deps_and_cli)
- [Clojure core library] (https://clojure.org/api/cheatsheet)
- [Clojure best practices] (https://gist.github.com/xeqi/681753)
- [Clojure programmering] (https://clojure.org/guides/learn/programming)