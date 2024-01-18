---
title:                "Capitalizzazione di una stringa"
html_title:           "Lua: Capitalizzazione di una stringa"
simple_title:         "Capitalizzazione di una stringa"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/lua/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Che cosa & Perché?

Il "capitalizing" di una stringa , è un tipo di stringa che ha la prima lettera di ogni parola in maiuscolo. I programmatori spesso utilizzano questa formattazione per rendere più leggibili e organizzate le stringhe di testo, in particolare nelle interfacce utente e nei documenti di testo.

# Come fare:

```Lua
-- Esempio di capitalizing di una stringa

stringa = "questa è una stringa di prova"
print(string.upper(stringa))
-- Output: "QUESTA È UNA STRINGA DI PROVA"

-- Esempio di capitalizing di una stringa con l'uso della funzione capitalize()
stringa = "prova con capitalize()"
print(string.capitalize(stringa))
-- Output: "Prova con capitalize()"
```

# Approfondimento:

Il concetto di capitalizing deriva dal termine "capitalize" che significa mettere in maiuscolo la prima lettera di una parola. Questo tipo di formattazione è spesso utilizzata nelle lingue occidentali e deriva dalle convenzioni tipografiche nelle quali le lettere maiuscole sono utilizzate all'inizio delle frasi e nei nomi propri.

Un'alternativa all'utilizzo della funzione capitalize() è quella di utilizzare la funzione upper(), che mette in maiuscolo l'intera stringa. Inoltre, è possibile creare una funzione personalizzata per il capitalizing che permetta di specificare quali lettere mettere in maiuscolo e quali lasciare in minuscolo.

Per quanto riguarda l'implementazione, la funzione capitalize() utilizza gli standard ASCII per determinare se una lettera è maiuscola o minuscola. Inoltre, questa funzione è case-sensitive, quindi è importante fare attenzione al tipo di lettere presenti nella stringa.

# Vedi anche:

- [Funzione string.capitalize()](https://www.lua.org/pil/20.2.html)
- [Funzione string.upper()](https://www.lua.org/pil/20.2.html)
- [Codice sorgente di Lua per approfondimenti](https://www.lua.org/source/5.3/)