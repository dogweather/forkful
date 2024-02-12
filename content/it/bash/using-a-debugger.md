---
title:                "Utilizzo di un debugger"
aliases:
- it/bash/using-a-debugger.md
date:                  2024-01-26T03:47:24.742508-07:00
model:                 gpt-4-0125-preview
simple_title:         "Utilizzo di un debugger"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/using-a-debugger.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Usare un debugger in Bash significa sfruttare strumenti per testare e trovare problemi nei vostri script, come catturare bug che fanno crashare il vostro codice o che lo fanno comportare in modo subdolo. I programmatori lo fanno perché è molto più intelligente catturare gli errori prima che provochino danni in un ambiente live.

## Come fare:
Bash non è dotato di un debugger integrato come alcuni altri linguaggi, ma è possibile utilizzare comandi integrati come `set -x` per tracciare cosa sta succedendo. Oppure, per un upgrade, c'è `bashdb`, un vero e proprio debugger per eseguire passo dopo passo il vostro codice. Ecco un'anteprima:

```Bash
# Usare set -x per il debug
set -x
echo "Inizio del debug"
my_var="Ciao, Mondo del Debug!"
echo $my_var
set +x

# Usare bashdb
# Installare bashdb con il gestore di pacchetti, es., apt, yum, brew.
# Eseguire il debug di uno script chiamato my_script.sh:
bashdb my_script.sh
```

Output quando si esegue con `set -x`:
```Bash
+ echo 'Inizio del debug'
Inizio del debug
+ my_var='Ciao, Mondo del Debug!'
+ echo 'Ciao, Mondo del Debug!'
Ciao, Mondo del Debug!
+ set +x
```

## Approfondimento
Storicamente, il debug degli script Bash significava disseminare il proprio codice di istruzioni `echo`. Ma poi è arrivato `set -x`, offrendoci uno sguardo all'esecuzione in tempo reale senza stampati manuali. E per coloro che desiderano più controllo, è apparso il debugger `bashdb`, ispirato al debugger gdb per C/C++.

Per quanto riguarda le alternative, oltre ai comandi `set` (`-x`, `-v`, `-e`), altre opzioni includono il reindirizzamento dell'output su un file per l'analisi o l'utilizzo di strumenti esterni come ShellCheck per l'analisi statica.

Per quanto riguarda l'implementazione, `set -x` è facile; è un'opzione nativa di Bash che stampa i comandi e i loro argomenti man mano che vengono eseguiti. `bashdb`, d'altra parte, permette di esaminare il codice passo dopo passo, impostare punti di interruzione e valutare espressioni - cose che vi danno una possibilità di lotta contro bug più elusivi.

## Vedi Anche
- Progetto Debugger Bash: http://bashdb.sourceforge.net/
- "Pro Bash Programming" di Chris Johnson e Jayant Varma per lo scripting avanzato.
- ShellCheck per l'analisi statica: https://www.shellcheck.net/
