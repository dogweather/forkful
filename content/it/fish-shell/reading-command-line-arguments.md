---
title:                "Fish Shell: Lettura degli argomenti della riga di comando"
simple_title:         "Lettura degli argomenti della riga di comando"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Perché

Se stai iniziando ad imparare la programmazione in Fish Shell, potresti chiederti perché è importante studiare come leggere gli argomenti dalla riga di comando. In realtà, questa tecnica è molto utile per automatizzare il processo di esecuzione dei tuoi script e per rendere più flessibile il tuo codice.

## Come fare

Per leggere gli argomenti dalla riga di comando in Fish Shell, puoi utilizzare la variabile $argv. Questa variabile contiene un elenco dei valori passati come argomenti al tuo script, separati da spazio. Ad esempio, se il tuo script si chiama "mio_script.fish" e lo esegui come `mio_script.fish arg1 arg2`, il valore di $argv sarà `arg1 arg2`. 

```Fish Shell
#!/usr/bin/env fish

echo "Hai passato questi argomenti: $argv"
```

L'output di questo script sarebbe `Hai passato questi argomenti: arg1 arg2`.

È possibile utilizzare un ciclo for per iterare attraverso i singoli argomenti come mostrato nell'esempio seguente:

```Fish Shell
#!/usr/bin/env fish

for arg in $argv
    echo "$arg"
end
```

L'output di questo script sarebbe:

```
arg1
arg2
```

## Profondità

Oltre alla variabile $argv, è possibile utilizzare la funzione fish_read_args per leggere gli argomenti dalla riga di comando. Questa funzione ha opzioni aggiuntive che ti consentono di specificare un pattern di matching per gli argomenti e di gestire gli errori.

Per ulteriori informazioni su come leggere gli argomenti dalla riga di comando in Fish Shell, puoi consultare la documentazione ufficiale sul [sito web di Fish Shell](https://fishshell.com/docs/current/cmds/read_args.html).

## Vedi anche

* [Documentazione ufficiale di Fish Shell su read_args](https://fishshell.com/docs/current/cmds/read_args.html)
* [Tutorial su Fish Shell](https://dev.to/christopherkade/fish-functions-and-how-to-use-them-1-easiest-shell-functions-to-get-started-with-shell-scripting-4fo6) 
* [Come creare un alias in Fish Shell](https://medium.com/platformer-blog/improve-your-shell-alias-bd4074b47986)