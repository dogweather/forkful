---
title:                "Att använda en debugger"
aliases:
- sv/clojure/using-a-debugger.md
date:                  2024-01-26T03:48:19.193934-07:00
model:                 gpt-4-0125-preview
simple_title:         "Att använda en debugger"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/using-a-debugger.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att använda en debugger innebär att du utrustar dig själv med ett förstoringsglas för att granska din kod. Programmerare gör detta för att krossa buggar, förstå flöden och försäkra sig om att deras logik fungerar som förväntat.

## Hur man gör:
Clojure stödjer sig på Java Virtual Machine (JVM), så mycket av felsökningen sker med Java-verktyg. Ett sådant verktyg är `CIDER`, ett kraftfullt paket för Clojure-utveckling i Emacs, som har gedigna felsökningsförmågor. Låt oss dyka in:

```clojure
;; Först, anslut till ett Clojure-projekt inuti Emacs med CIDER
M-x cider-jack-in

;; Ställ in en brytpunkt
;; Navigera till raden i din Clojure-kod som du vill inspektera och
;; tryck "C-c M-b" eller exekvera:
M-x cider-debug-defun-at-point

;; När koden körs kommer du att träffa på brytpunkten. CIDER kommer att uppmana dig med:
;; 1. n för att gå till nästa logiska steg i utförandet,
;; 2. c för att fortsätta utförandet till nästa brytpunkt,
;; 3. q för att avsluta felsökningen.

;; Inspektera lokala variabler vid brytpunkten
;; Medan vid en brytpunkt, skriv:
locals

;; Du kommer att se en lista över lokala variabler och deras värden utskrivna i minibuffern.
```
Exempel på utskrift kan se ut som:
```clojure
{:x 10, :y 20, :resultat 200}
```

## Fördjupning
Debuggern är ett verktyg lika gammalt som kullarna i datatermer. Termen "bugg" myntades tillbaka i de tidiga dagarna av dataåldern när ett faktiskt insekt orsakade ett fel genom att kortsluta en krets i en maskin.

Medan `CIDER` är fantastiskt för Emacs-entusiaster, finns det alternativ för Clojure-felsökning. Till exempel, användningen av IntelliJ med Cursive-pluginet kan erbjuda en mer GUI-driven felsökningsupplevelse. Dessutom kan du använda den inbyggda Leiningen eller tools.deps för att kontrollera processflödet vid felsökning.

Bakom kulisserna manipulerar dessa debuggerar ofta bytekoder, utför utvärderingar i dedikerade nREPL-sessioner och erbjuder stack trace-inspektion. De utnyttjar den underliggande JVM:s kapaciteter, genom att tappa in i rikedomen av Javas felsökningsramverk.

## Se även
- [CIDER Debugger-dokumentation](https://docs.cider.mx/cider/debugging/debugger.html)
- [Cursive Debugger](https://cursive-ide.com/userguide/debugging.html)
- [Leiningen för Automation och Felsökning](https://leiningen.org/)
- [tools.deps.alpha för mer kontroll](https://github.com/clojure/tools.deps.alpha)
