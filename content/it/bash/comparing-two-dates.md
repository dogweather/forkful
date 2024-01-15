---
title:                "Confrontare due date"
html_title:           "Bash: Confrontare due date"
simple_title:         "Confrontare due date"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Perché
Ci sono molte ragioni per voler confrontare due date nella programmazione Bash. Innanzitutto, può essere utile per verificare la validità di una data inserita dall'utente o per controllare se una data è successiva o precedente a un'altra.

## Come fare
L'operatore di comparazione per le date in Bash è `[[ $data1 operatore $data2 ]]`, dove "operatore" può essere uno di questi: `-eq` (uguaglianza), `-ne` (disuguaglianza), `-lt` (minore di), `-le` (minore o uguale a), `-gt` (maggiore di) o `-ge` (maggiore o uguale a). Vediamo alcuni esempi pratici:

```
Bash
data1="2021-06-02"
data2="2021-06-01"

if [[ $data1 -gt $data2 ]]; then
    echo "$data1 è successiva a $data2"
else
    echo "$data1 è precedente o uguale a $data2"
fi
```
Output:
```
2021-06-02 è successiva a 2021-06-01
```
In questo esempio, le date sono state assegnate alle variabili `data1` e `data2`, e quindi è stato utilizzato l'operatore `-gt` per verificare se `data1` è successiva a `data2`. In caso affermativo, viene stampato un messaggio appropriato.

```
Bash
data1="2021-06-05"
data2="2021-06-01"

if [[ $data1 -le $data2 ]]; then
    echo "$data1 è precedente o uguale a $data2"
else
    echo "$data1 è successiva a $data2"
fi
```
Output:
```
2021-06-05 è successiva a 2021-06-01
```
In questo secondo esempio, l'operatore `-le` viene utilizzato per verificare se `data1` è precedente o uguale a `data2`.

Ci sono anche altri modi per confrontare le date in Bash, ad esempio utilizzando il comando `test` o utilizzando il formato delle date Unix. Tuttavia, l'utilizzo degli operatori di comparazione è il metodo più semplice e diretto.

## Approfondimenti
Per un confronto più dettagliato tra due date, si può utilizzare il comando `date` combinato con il comando `diff`, che mostra la differenza in termini di giorni, ore, minuti e secondi tra due date specifiche. Ad esempio, digitando `date -d "2021-06-05" -d "2021-06-01" -u`, verrà visualizzato il seguente output:

```
4 days, 0:00:00
```

Ci sono anche molte librerie e framework disponibili per semplificare la manipolazione e il confronto delle date in Bash, ad esempio `dateutils` e `datecalc`.

## Vedi anche
- [Manuale Bash-Operazioni aritmetiche](https://www.santacruzlinux.org/man/man1/bash-Arithmetic-Operations.html)
- [Documentazione del comando Data di Linux](https://tldp.org/LDP/Bash-Beginners-Guide/html/sect_02_03.html)
- [Libreria dateutils per Bash](https://www.gnu.org/software/dateutils/manual/dateutils.html)
- [Libreria datecalc per Bash](https://gitlab.com/coffeetimeapps/tool-by-svn-and-git/-/tree/master/GitBash/AndroidCompleteDateCalculatorByAhamadUL/backupApkFiles/datecalculator/datecalculatordatecalculator/WeekJavaDetail)