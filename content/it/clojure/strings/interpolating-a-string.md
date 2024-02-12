---
title:                "Interpolazione di una stringa"
date:                  2024-01-20T17:50:32.143689-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolazione di una stringa"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Interpolare una stringa significa inserire dinamicamente valori specifici all'interno di una stringa predefinita. I programmatori lo usano per creare messaggi personalizzati, per esempio, mostrando il nome di un utente o i risultati del calcolo.

## Come fare:
```
Clojure
; Usare `str` per concatenare le stringhe
(def nome "Marco")
(println (str "Ciao, " nome "!"))

; Uso di `format`
(println (format "Benvenuto %s, oggi è il %d!" "Luca" 14))

; Uso delle backquote e la macro `str`
(defn saluto [nome giorno]
  `(str "Buongiorno " ~nome ", come va il " ~giorno "?"))
(println (saluto "Giovanni" "Mercoledì"))
```
Output:
```
Ciao, Marco!
Benvenuto Luca, oggi è il 14!
Buongiorno Giovanni, come va il Mercoledì?
```

## Approfondimento
Storicamente, l'interpolazione di stringhe non era direttamente supportata in Clojure come in altri linguaggi, si usava la concatenazione o la funzione `format`. Oggi restano metodi validi, ma alternative più moderne sono disponibili tramite librerie esterne che offrono una sintassi più pulita o attraverso macro personalizzate.

Le macro in Clojure sono un potente strumento che, tra molte altre cose, possono essere usate per creare funzionalità personalizzate di interpolazione. Queste sono a livello di compilazione e permettono un'interpolazione più leggibile ed efficiente.

È inoltre possibile utilizzare vettori e mappe con `str` o `format` per ottenere effetti di interpolazione.

## Vedi Anche
- Documentazione ufficiale di Clojure [https://clojure.org/](https://clojure.org/)
- Clojure `str` [https://clojuredocs.org/clojure.core/str](https://clojuredocs.org/clojure.core/str)
- Clojure `format` [https://clojuredocs.org/clojure.core/format](https://clojuredocs.org/clojure.core/format)
