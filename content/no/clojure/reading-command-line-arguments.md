---
title:                "Leser kommandolinjeargumenter"
html_title:           "Clojure: Leser kommandolinjeargumenter"
simple_title:         "Leser kommandolinjeargumenter"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hvorfor

Det kan være flere grunner til å lære om hvordan man leser og håndterer kommandolinjeargumenter i Clojure. For det første er det en viktig del av programmeringsspråket, og det er nyttig å kunne for å kunne utvikle komplekse og funksjonelle programmer. Det kan også være nyttig for å forbedre effektiviteten og hastigheten til programmer ved å tillate brukeren å gi input gjennom kommandolinjen.

## Hvordan

Det første trinnet for å lese kommandolinjeargumenter i Clojure er å importere biblioteket "clojure.tools.cli". Dette kan gjøres ved å legge til følgende kode i starten av programmet:

```Clojure
(require '[clojure.tools.cli :refer :all])
```

Deretter kan du bruke funksjonen "parse-opts" for å lese argumentene og lagre dem i en map. Se eksempelet under for å forstå hvordan dette fungerer:

```Clojure
(defn -main [& args]
  (let [options (parse-opts args
                  [["-n" "--name NAME" "Your name"]])]
    (println (str "Hello " (:name options) "!"))))

;; Hvis du kjører programmet fra kommandolinjen med argumentet "-n John", vil output være:
;; "Hello John!"
```

I dette eksempelet bruker vi kommandolinjealternativene "-n" og "--name", og vi lagrer den oppgitte verdien i en map under nøkkelen "name". Så kan vi bruke denne verdien i programmet vårt, for eksempel ved å skrive den ut med en tilpasset melding.

## Deep Dive

Det er også mulig å spesifisere forskjellige typer argumenter, for eksempel heltall, desimaltall, booleans eller filbaner. Dette gjøres ved å legge til en ekstra parameter i "parse-opts" funksjonen, som beskrevet under:

```Clojure
(defn -main [& args]
  (let [options (parse-opts args
                  [["-n" "--name NAME" "Your name"]
                   ["-a" "--age AGE" :parse-fn #(Integer/parseInt %) "Your age"]
                   ["-l" "--length LENGTH" :parse-fn #(Float/parseFloat %) "Your height in meters"]
                   ["-v" "--verbose" "Enable verbose mode"]
                   ["-f" "--file FILE" :parse-fn #(java.io.File. %) "File path"]])]
    (println (str "Hello " (:name options) "!"))
    (when (:verbose options)
      (println (str "Your age is " (:age options) " and your height is " (:length options) " meters."))
    (println (str "The given file is located at " (:file options))))))

;; Hvis du kjører programmet med argumentene "-n John -a 30 -l 1.8 -v -f /Users/John/Documents/sample.txt", vil output være:
;; "Hello John!"
;; "Your age is 30 and your height is 1.8 meters."
;; "The given file is located at /Users/John/Documents/sample.txt"
```

Som du kan se, har vi nå spesifisert forskjellige typer argumenter og lagret dem i riktige typer i map-en vår. Vi kan også se hvordan vi kan bruke argumentene i programmet vårt, for eksempel ved å sjekke om "-v" flagget er aktivert og skrive ut tilleggsinformasjon.

## Se Også

- [clojure.tools.cli Dokumentasjon](https://clojure.github.io/tools.cli/)

- [Clojure Argument Parsing Tutorial](http://clojure-doc.org/articles/tutorials/parsing_command_line_arguments_with_tools_cli.html)