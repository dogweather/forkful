---
title:                "Lettura degli argomenti della riga di comando"
html_title:           "Fish Shell: Lettura degli argomenti della riga di comando"
simple_title:         "Lettura degli argomenti della riga di comando"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

##Perché
Probabilmente sei qui perché vuoi imparare a utilizzare la Fish Shell e sei curioso di conoscere i suoi comandi. Leggere gli argomenti della riga di comando è un'abilità fondamentale per poter usare la Shell in modo efficace e veloce. Questo articolo ti spiegherà come farlo nel modo più semplice possibile.

##Come Fare
Per leggere gli argomenti della riga di comando nella Fish Shell, è necessario utilizzare il comando `read`, seguito da una variabile dove verranno salvati i dati inseriti dall'utente. Vediamo un esempio:

```Fish Shell
read -l nome
```

Questo comando aspetterà che l'utente inserisca il proprio nome e lo salverà nella variabile `nome`. Per stampare il nome inserito, possiamo utilizzare il comando `echo` seguito dal nome della variabile. Ad esempio:

```Fish Shell
echo $nome
```

Se vogliamo leggere più di un argomento nella stessa riga, possiamo utilizzare il flag `-a`, che ci permette di creare un array con i dati inseriti. Vediamo un esempio:

```Fish Shell
read -a preferiti
```

Con questo comando, l'utente potrà inserire più argomenti separati da spazi e verranno salvati nell'array `preferiti`. Possiamo poi stampare l'array utilizzando il comando `echo` seguito dal nome dell'array e dal numero dell'elemento che vogliamo visualizzare. Ad esempio:

```Fish Shell
echo $preferiti[1]
```

##Deep Dive
Ora che sai come leggere gli argomenti della riga di comando nella Fish Shell, è importante comprendere che è anche possibile assegnare dei valori di default alle variabili. Ad esempio:

```Fish Shell
read -l -i "Sconosciuto" nome
```

In questo modo, se l'utente non inserisce alcun valore, la variabile `nome` avrà come valore di default "Sconosciuto". Inoltre, possiamo anche specificare una lunghezza massima per l'input utilizzando il flag `-m`. Vediamo un esempio:

```Fish Shell
read -l -m 20 nome
```

In questo caso, l'utente può inserire al massimo 20 caratteri per il nome.

##Vedi Anche
- [Comandi di base della Fish Shell](https://fishshell.com/docs/current/tutorial.html#basic-commands)
- [Creare script nella Fish Shell](https://fishshell.com/docs/current/tutorial.html#scripting)