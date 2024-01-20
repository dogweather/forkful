---
title:                "Lettura degli argomenti della riga di comando"
html_title:           "Java: Lettura degli argomenti della riga di comando"
simple_title:         "Lettura degli argomenti della riga di comando"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Cos'è e perchè?

La lettura degli argomenti da linea di comando in Bash permette agli script di interagire con l'utente. Questa funzionalità rende gli script più flessibili e personalizzabili in base alle esigenze dell'utente.

## Come si fa:

Ecco un semplice script Bash che dimostra come leggere gli argomenti dalla linea di comando. Il comando `echo` è usato per stampare argomenti a schermo.

```Bash
#!/bin/bash
echo "Il primo argomento è: $1"
echo "Il secondo argomento è: $2"
echo "Il terzo argomento è: $3"
```

Ecco l'output del codice quando si passano tre argomenti (esempio1, esempio2, esempio3) allo script:

```Bash
Il primo argomento è: esempio1
Il secondo argomento è: esempio2
Il terzo argomento è: esempio3
```

## Approfondimento

La lettura degli argomenti da linea di comando esiste fin dall'origine dei sistemi Unix. Nel contesto storico, ci ha permesso di automatizzare compiti ripetitivi in modo più efficiente. È fondamentale ricordare che in Bash gli argomenti sono accessibili tramite i parametri posizionali `$1`, `$2`, `$3`, ecc.

Alternative alla lettura di argomenti da linea di comando includono l'uso di file di input o di variabili d'ambiente. Tuttavia, gli argomenti da linea di comando sono spesso la scelta preferita per la loro semplicità e flessibilità.

## Per saperne di più

Per ulteriori informazioni su come utilizzare Bash e gli argomenti della linea di comando, visita i seguenti link:

- [Manuale GNU Bash](https://www.gnu.org/software/bash/manual/bash.html)
- [Corso interattivo Bash](https://www.codecademy.com/learn/learn-the-command-line/modules/bash-scripting)
- [Guida alla programmazione bash](http://tldp.org/HOWTO/Bash-Prog-Intro-HOWTO.html)