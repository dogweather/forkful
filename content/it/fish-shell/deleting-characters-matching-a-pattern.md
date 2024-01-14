---
title:    "Fish Shell: Eliminazione di caratteri corrispondenti a un modello"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Perché

La cancellazione di caratteri corrispondenti a un determinato pattern può essere utile per semplificare e pulire il codice, rimuovendo informazioni non necessarie o errate. Può anche essere utilizzata per effettuare operazioni su stringhe o altre variabili che richiedono un filtro specifico.

## Come fare

Per cancellare i caratteri corrispondenti a un pattern nella Shell di Fish, è possibile utilizzare il comando `string match` seguito dal carattere `^` e dal pattern desiderato tra parentesi quadre. Ad esempio, se si vuole eliminare tutti i caratteri numerici da una stringa, si può utilizzare il seguente comando:
```
string match -r '[0-9]' stringa1 stringa2
```
Dove "stringa1" e "stringa2" rappresentano le stringhe su cui si vuole effettuare l'operazione. Il risultato sarà una nuova stringa che contiene solo i caratteri non numerici presenti nelle stringhe originali.

## Approfondimento

Il comando `string match` utilizza l'espressione regolare per cercare i caratteri corrispondenti al pattern specificato. Tuttavia, è possibile anche utilizzare altri caratteri al posto di `^`, come `$` per eliminare i caratteri alla fine della stringa o `*` per eliminare più di un carattere corrispondente. Inoltre, è possibile specificare più pattern tra parentesi quadre per eliminare più caratteri contemporaneamente, ad esempio `[0-9a-z]`.

Inoltre, il comando `string match` può essere utilizzato in combinazione con altri comandi per effettuare operazioni più complesse. Ad esempio, si può utilizzare il comando `string sub` per sostituire i caratteri eliminati con un altro carattere o stringa specificata.

## Vedi anche

- [Documentazione ufficiale di Fish Shell](https://fishshell.com/docs/current/cmds/string.html#match)
- [Tutorial sulle espressioni regolari in Bash](https://www.shell-tips.com/regex-tutorial)