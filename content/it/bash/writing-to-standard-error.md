---
title:                "Bash: Scrivere su standard error"
simple_title:         "Scrivere su standard error"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Perché

Scrivere su standard error, o stderr, è utile per visualizzare errori e avvisi durante l'esecuzione di uno script o di un programma. È un modo per tenere traccia degli eventuali problemi che potrebbero verificarsi e per debuggare il codice.

## Come fare

Per scrivere su stderr in Bash, è sufficiente utilizzare il segno ">&2" dopo il comando che si desidera stampare. Ad esempio:
```Bash
echo "Errore: il file non è stato trovato" >&2
```
In questo modo, il testo del messaggio verrà stampato su stderr anziché su stdout.

## Approfondimento

Come accennato prima, scrivere su stderr è particolarmente utile per la fase di debugging. In questo modo, è possibile distinguere tra i messaggi di errore e di output generico e concentrarsi sui problemi reali che si verificano nello script o nel programma.

Inoltre, scrivere su stderr è anche importante quando si utilizza il piping in Bash. Se si vuole che l'output di un comando venga utilizzato come input di un altro, ma si desidera comunque visualizzare eventuali messaggi di errore, è necessario scrivere su stderr.

Invece di utilizzare ">&2" dopo ogni comando, è possibile creare una funzione per scrivere su stderr in modo più efficiente. Ad esempio:
```Bash
err() {
	echo "$@" >&2
}
```
In questo modo, basterà chiamare la funzione "err" seguita dal messaggio di errore per scrivere su stderr.

## Vedi anche

- [Il piping in Bash](https://linuxconfig.org/learning-bash-testing-on-standard-input)
- [Come utilizzare stderr e il piping in Bash](https://www.linuxjournal.com/content/bash-redirections-using-stderr-other-languages)