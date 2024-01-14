---
title:    "Bash: Verificare se esiste una directory"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Perché

Quando si scrive uno script Bash, può capitare di dover verificare se una determinata directory esiste prima di effettuare una determinata operazione. Questo è importante perché, nel caso in cui la directory non esista, lo script potrebbe generare errori o comportamenti indesiderati.

## Come Fare

Per verificare se una directory esiste, possiamo utilizzare il comando `test` seguito dall'opzione `-d`, che controlla se un determinato percorso corrisponde a una directory. Ecco un esempio:

```Bash
test -d /percorso/directory/esistente
```

Il comando restituirà 0 (successo) se la directory esiste, mentre restituirà un valore diverso da 0 (errore) se la directory non esiste. Possiamo utilizzare questo risultato all'interno di uno statement `if` per gestire diversi scenari.

```Bash
if test -d /percorso/directory/esistente
then
    echo "La directory esiste"
else
    echo "La directory non esiste"
fi
```

In questo esempio, se il comando `test` restituisce 0, lo statement `then` eseguirà il codice per indicare che la directory esiste, altrimenti il codice nell'`else` verrà eseguito per indicare che la directory non esiste.

## Approfondimento

È importante notare che il comando `test` può anche essere scritto in modo abbreviato utilizzando le parentesi tonde `( )` anziché gli statement `if` e `else`.

```Bash
test -d /percorso/directory/esistente && echo "La directory esiste"
```

In questo caso, se la directory esiste, verrà eseguito il comando `echo` per indicare che la directory esiste. In caso contrario, il comando verrà ignorato.

Una nota importante è che il comando `test` può anche essere utilizzato per verificare se un file esiste, utilizzando l'opzione `-f` invece di `-d`.

## Vedi Anche

- [Comando test su Linux Manpages](https://linux.die.net/man/1/test)
- [Tutorial su Bash scripting su Linuxize](https://linuxize.com/post/bash-scripting-tutorial/)