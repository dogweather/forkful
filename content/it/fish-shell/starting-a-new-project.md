---
title:                "Avviare un nuovo progetto"
html_title:           "Fish Shell: Avviare un nuovo progetto"
simple_title:         "Avviare un nuovo progetto"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Perché

Ciao a tutti! Siete pronti a scoprire il mondo della programmazione con Fish Shell? Magari ti stai chiedendo perché dovresti intraprendere questo nuovo progetto, ma la risposta è semplice: Fish Shell offre una sintassi più moderna e intuitiva rispetto ai classici terminali come Bash o Zsh. Inoltre, ha una vasta gamma di funzionalità che lo rendono uno strumento potente per gestire le tue attività di programmazione. Quindi, se vuoi rendere la tua esperienza di codifica più efficiente e divertente, continua a leggere!

## Come fare

Per iniziare a utilizzare Fish Shell, prima di tutto assicurati di averlo correttamente installato sul tuo sistema operativo. Una volta fatto ciò, puoi accedere al tuo prompt dei comandi e cominciare ad esplorare le sue funzionalità.

```
Fish Shell version 3.2.2
```

### Prompt personalizzato

Una delle prime cose che noterai è il prompt personalizzabile di Fish Shell. Puoi modificarlo a tuo piacimento, aggiungendo colori, emoji, informazioni sul sistema e altro ancora. Ad esempio, se vuoi che il tuo prompt mostri il nome utente, il percorso corrente e la versione di Fish Shell, puoi utilizzare il seguente comando:

```
set -Ux fish_prompt "╭──({whoami}@[{hostname}]) $(basename ({pwd}))$ fish_shell version {fish_version}╰─➤ "
```

Ecco come apparirà il tuo nuovo prompt:

```
╭──(username@my-computer) Documents$ fish_shell version 3.2.2╰─➤ 
```

### Completamento automatico

Fish Shell ha un sistema di completamento automatico estremamente efficiente che ti aiuterà a risparmiare tempo durante la scrittura dei comandi. Ad esempio, se devi spostarti in una cartella specifica, puoi semplicemente iniziare a digitare il nome della cartella e premere il tasto "Tab" per completare automaticamente il percorso.

```
cd m<TAB>
```

Fish Shell completerà automaticamente il resto del nome della cartella, nel caso in cui ci siano più cartelle con lo stesso inizio. Inoltre, puoi utilizzare il comando `bind` per definire i tuoi completamenti personalizzati per risparmiare ancora più tempo.

### Gestione delle variabili

Fish Shell offre una gestione delle variabili molto più semplice rispetto ad altri terminali. Per definire una variabile, basta utilizzare il comando `set`, seguito dal nome della variabile e il suo valore. Per accedere alle variabili, usa il simbolo del dollaro `$` come in altri linguaggi di programmazione.

```
set my_variable "Hello world"
echo $my_variable
```

Fish Shell offre anche una sintassi più pulita per le variabili di ambiente, utilizzando il formato `VAR=value` anziché `export VAR=value`.

## Deep Dive

Fish Shell è un progetto open-source e in continua evoluzione, supportato da una comunità attiva di sviluppatori. Se vuoi contribuire al suo sviluppo o segnalare bug, puoi dare un'occhiata al suo repository su GitHub. Inoltre, puoi anche personalizzare ulteriormente il tuo Fish Shell utilizzando i temi disponibili nella galleria dei temi.

Tieni presente che Fish Shell potrebbe richiedere un po' di tempo per abituarsi, soprattutto se sei abituato ai comandi di altri terminali. Ma una volta che imparerai a sfruttarne le funzionalità, non tornerai più indietro!

## Vedi anche

- [Fish Shell documentazione ufficiale](https://fishshell.com/docs/current/index.html)
- [Fish Shell repository su GitHub](https://github.com/fish-shell/fish-shell)
- [Galleria dei temi Fish Shell](https://github.com/oh-my-fish/theme-bobthefish)