---
title:    "Fish Shell: Confronto tra due date"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Perché
A volte, quando si lavora con dati e informazioni temporali, può essere utile confrontare due date per vedere quale sia più recente o se ci sia una differenza di tempo tra di esse. Utilizzando il Fish Shell, è possibile eseguire facilmente questa operazione utilizzando poche righe di codice.

## Come fare
Per iniziare, apriamo il terminale e assicuriamoci di avere il Fish Shell installato e configurato come shell predefinita. Successivamente, creiamo un nuovo file di script con estensione ".fish" e inseriamo il seguente codice:

```
Fish Shell F1: 
#!/bin/fish

# Definiamo due variabili contenenti le due date da confrontare
set first_date "2021-03-10"
set second_date "2020-05-18"

# Utilizziamo il comando `date` per trasformare le date in formato "epoca", che ci permette di confrontarle
# (se le vostre date sono già in formato "epoca", potete saltare questo passaggio)
set first_epoch (date -f "%Y-%m-%d" -u $first_date "+%s")
set second_epoch (date -f "%Y-%m-%d" -u $second_date "+%s")

# Confrontiamo le date e stampiamo la differenza in giorni
if test $first_epoch -gt $second_epoch
        echo "La prima data è più recente della seconda di (( $first_epoch - $second_epoch) / 86400 )) giorni."
else if test $first_epoch -lt $second_epoch
        echo "La seconda data è più recente della prima di (( $second_epoch - $first_epoch) / 86400 )) giorni."
else
        echo "Le due date sono uguali."
end
```

Salviamo il file e assegniamogli i permessi di esecuzione con il comando `chmod +x nomefile.fish`. Ora eseguiamo lo script con `./nomefile.fish` e dovremmo ottenere un output simile a questo:

```
La prima data è più recente della seconda di 300 giorni.
```

## Approfondimento
Per chi volesse familiarizzare di più con la gestione delle date nel Fish Shell, è possibile utilizzare il comando `man date` per ottenere una lista completa di opzioni e formattazioni disponibili. Inoltre, è possibile utilizzare altri strumenti come `calendar` o `timedatectl` per ottenere informazioni sulle date e i tempi di sistema.

## Vedi anche
- [Documentazione ufficiale del Fish Shell](https://fishshell.com/docs/current/index.html)
- [Guida introduttiva al Fish Shell](https://www.digitalocean.com/community/tutorials/how-to-install-and-use-the-fish-shell-on-a-cloud-server)
- [Siti utili per imparare il Fish Shell](https://www.opensource.com/article/18/3/getting-started-fish-shell)