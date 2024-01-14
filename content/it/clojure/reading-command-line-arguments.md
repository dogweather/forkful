---
title:    "Clojure: Lettura degli argomenti dalla linea di comando"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Perché

Mentre si scrive codice in Clojure, ci possono essere situazioni in cui si vuole essere in grado di leggere gli argomenti della riga di comando forniti dall'utente. Ciò può essere utile quando si vuole dare all'utente un controllo più diretto sul programma o quando si vuole facilitare la personalizzazione del programma.

## Come fare

Per leggere gli argomenti della riga di comando in Clojure, è possibile utilizzare la funzione `command-line-args` che restituisce una lista di stringhe contenente gli argomenti forniti dall'utente. Ad esempio:

```Clojure
(def args (command-line-args))
(println args)
```

L'output di questo codice dipenderà dagli argomenti forniti dall'utente. Ad esempio, se l'utente fornisce `java -jar nome_programma arg1 arg2`, l'output sarà:

```
[arg1 arg2]
```

È possibile utilizzare la funzione `nth` per accedere a un argomento specifico nella lista. Ad esempio, per ottenere il primo argomento, si può scrivere:

```Clojure
(nth args 0)
```

Questo restituirà `arg1`.

Inoltre, Clojure offre anche una libreria chiamata `cli.args` che semplifica la lettura degli argomenti della riga di comando e fornisce funzionalità aggiuntive come la validazione degli argomenti. Per utilizzarla, è necessario importarla:

```Clojure
(require 'cli.args)
```

Si possono quindi definire i parametri e la loro validazione utilizzando la funzione `cli.spec/defcli`.

## Approfondimento

La funzione `command-line-args` utilizza il valore di una proprietà di sistema chiamata `"clojure.main.opts"`. Questa proprietà viene settata dallo script `clojure` utilizzato per avviare i programmi Clojure. Questo significa che gli argomenti possono essere letti solo durante l'esecuzione dello script `clojure`. Se si desidera leggere gli argomenti da un'altra classe Java, è possibile utilizzare la proprietà di sistema `"clojure.args"`.

Inoltre, è importante notare che gli spazi e i caratteri speciali negli argomenti vengono preservati come stringhe. Se si desidera elaborarli in modo diverso, è possibile utilizzare la funzione `shlex-split` della libreria `clojure.string`.

## Vedi anche

- [Documentazione ufficiale su command-line-args](https://clojure.org/reference/repl_and_main#_command_line_arguments)
- [Aritcolo di blog di Nashorn Clojure su cli.args](https://blog.nashorn.su/2016/07/02/0710-cli-args.html)
- [Documentazione ufficiale su cli.args](https://github.com/clojure/tools.cli)