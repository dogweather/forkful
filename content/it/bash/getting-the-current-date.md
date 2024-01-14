---
title:                "Bash: Ottenere la data corrente"
simple_title:         "Ottenere la data corrente"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Perché

La data corrente è un'informazione molto utile da avere quando si lavora con script di programmazione Bash. Essa può essere utilizzata per creare nomi di file dinamici, per effettuare backup o per organizzare le attività in base al giorno corrente.

## Come fare

Per ottenere la data corrente in Bash, è possibile utilizzare il comando `date`. Le opzioni più comuni sono `+%d` per il giorno, `+%m` per il mese, `+%Y` per l'anno e `+%H:%M:%S` per l'ora, i minuti e i secondi.

```Bash
$ date +%d
04
$ date +%Y-%m-%d
2021-01-04
```
Per ulteriori opzioni e formattazioni, è possibile consultare il manuale di Bash o eseguire il comando `date --help`.

## Approfondimento

Il comando `date` si basa sull'orologio di sistema per ottenere la data corrente. Se si vuole ottenere la data in un fuso orario diverso da quello del sistema, è possibile utilizzare l'opzione `-u` per ottenere la data universale coordinata (UTC).

```Bash
$ date
lun  4 gen 2021, 22:20:41 CET
$ date -u
lun  4 gen 2021, 21:20:41 UTC
```

Inoltre, è possibile specificare una data diversa dalla corrente utilizzando l'opzione `-d`.

```Bash
$ date
lun  4 gen 2021, 22:23:31 CET
$ date -d "2 months ago"
ven  4 set 2020, 22:23:31 CEST
```

Questo comando è particolarmente utile quando si lavora con script che devono eseguire attività in base a date specifiche.

## Vedi anche

- [Manuale di Bash](https://www.gnu.org/software/bash/manual/)
- [Documentazione di date](https://www.gnu.org/software/coreutils/manual/html_node/Date-invocation.html)
- [Tutorial su Bash](https://www.tutorialspoint.com/unix/shell_scripting.htm)