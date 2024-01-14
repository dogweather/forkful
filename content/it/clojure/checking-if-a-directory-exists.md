---
title:    "Clojure: Verifica dell'esistenza di una directory"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Perché

Molte volte, quando si lavora con programmi di automazione come Clojure, ci può capitare di dover controllare se una determinata directory esiste prima di eseguire delle operazioni su di essa. Questo è importante perché ci permette di evitare errori e gestire in modo efficiente l'esecuzione del codice.

## Come Fare

Il modo più semplice per verificare l'esistenza di una directory in Clojure è utilizzare la funzione `clojure.java.io/file`. Questa funzione prende come parametro il percorso della directory e restituisce un oggetto di tipo `File`.

```Clojure
(def dir (clojure.java.io/file "/percorso/directory"))
```

Per controllare se la directory esiste, possiamo utilizzare la funzione `exists?` sull'oggetto `File`.

```Clojure
(.exists? dir)
```

Se la directory esiste, la funzione restituirà `true`, altrimenti restituirà `false`.

Ecco un esempio completo con output:

```Clojure
(def dir (clojure.java.io/file "/percorso/directory"))

(println "La directory esiste? " (.exists? dir))
```

Output:

`La directory esiste? true`

## Approfondimento

Se vogliamo andare più in profondità, possiamo esplorare altre opzioni per verificare l'esistenza di una directory in modo più preciso.

Una di queste opzioni è utilizzare la funzione `fs-exists?` dal namespace `clojure.java.io`, che utilizza una chiamata al sistema operativo per verificare l'esistenza della directory. Questa funzione è utile soprattutto se stiamo lavorando su piattaforme diverse da Windows, come Linux o Mac.

Un altro approccio potrebbe essere utilizzare la libreria `fs` per interagire con il filesystem in modo più potente. Ad esempio, possiamo utilizzare la funzione `fs/lstat` per ottenere informazioni dettagliate sulla directory, come la data di creazione o l'ultimo accesso.

## Vedi Anche

- [Documentazione ufficiale di `clojure.java.io`](https://clojure.github.io/clojure/clojure.java.io-api.html)
- [Libreria `fs` per Clojure](https://github.com/alandipert/fs)
- [Altri approfondimenti sulla gestione del filesystem in Clojure](https://purelyfunctional.tv/mini-guide/filesystem-clojure/)