---
title:                "Fish Shell: Verifica dell'esistenza di una cartella"
programming_language: "Fish Shell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Perché
Se stai scrivendo script o programmi con Fish Shell, potresti dover verificare se una directory esiste prima di svolgere un'azione su di essa. Questo è utile per evitare errori e ottenere un codice più sicuro ed efficiente.

## Come 
Per controllare se una directory esiste con Fish Shell, è possibile utilizzare il comando `test` seguito dalla flag `-d` per indicare che si sta cercando una directory. Ad esempio: 
```
Fish Shell 
if test -d /percorso/directory
  echo "La directory esiste!"
end
```
Se la directory esiste, il messaggio "La directory esiste!" verrà stampato nel terminale. Altrimenti, il comando non restituirà nulla.

## Deep Dive
Per capire meglio come funziona questo controllo, è importante sapere che `test` cerca il file o la directory specificata e restituisce `true` se lo trova. Inoltre, la flag `-d` indica che si sta cercando appunto una directory, altrimenti si può utilizzare `-f` per controllare l'esistenza di un file.

È anche possibile utilizzare il comando `count` per verificare il numero di oggetti presenti nella directory. Ad esempio: 
```
Fish Shell 
set number (count /percorso/directory/*)
echo "$number oggetti nella directory"
```
Questo restituirà il numero di oggetti presenti nella directory specificata.

## See Also
- [Documentazione ufficiale di Fish Shell](https://fishshell.com/docs/current/index.html#test)
- [Tutorial su Fish Shell per principianti](https://blog.mockingbot.com/2020/01/28/introducing-fish-shell-to-beginners/)
- [Altri comandi utili di Fish Shell](https://www.cyberciti.biz/tips/finding-bash-shell-array-length-elements.html)