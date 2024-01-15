---
title:                "Concatenazione di stringhe"
html_title:           "Bash: Concatenazione di stringhe"
simple_title:         "Concatenazione di stringhe"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

## Perché

Se sei nuovo al mondo della programmazione, potresti chiederti perché dovresti imparare a concatenare stringhe in Bash. La risposta è semplice: concatenare stringhe è un'operazione molto comune e utile che ti aiuterà a gestire al meglio i tuoi script e programmi.

## Come fare

Per concatenare stringhe in Bash, puoi utilizzare l'operatore `+` o il comando `printf`. Ecco un esempio di codice che utilizza l'operatore `+`:

```Bash
first_name="Maria"
last_name="Rossi"
full_name=$first_name+$last_name
echo $full_name # output: Maria+Rossi
```

Ecco invece un esempio che utilizza il comando `printf`:

```Bash
first_name="Maria"
last_name="Rossi"
printf "Il tuo nome completo è %s %s.\n" $first_name $last_name # output: Il tuo nome completo è Maria Rossi.
```
Entrambe le soluzioni ti permettono di concatenare più stringhe e di ottenere un risultato finale come desideri. È importante notare che quando utilizzi l'operatore `+`, le stringhe verranno semplicemente unite senza spazi o caratteri aggiuntivi, mentre con `printf` puoi gestire l'output in modo più preciso.

## Approfondimento

Per concatenare stringhe in modo più avanzato, puoi utilizzare anche il comando `concat`. Questo comando ti consente di unire stringhe con spazi o altri caratteri specificati.

Ecco un esempio di codice che utilizza `concat`:

```Bash
first_name="Maria"
last_name="Rossi"
full_name=$(concat " " $first_name $last_name)
echo $full_name # output: Maria Rossi
```

Un altro metodo per concatenare stringhe è utilizzare l'operatore `.=`. Questo operatore ti consente di aggiungere una stringa alla fine di un'altra stringa senza sovrascriverla.

Ecco un esempio di codice che utilizza l'operatore `.=`:

```Bash
name="Marco"
name.="Rossi"
echo $name # output: MarcoRossi
```

Inoltre, puoi anche utilizzare una combinazione di stringhe e variabili per ottenere un risultato finale più dinamico e personalizzato. Non c'è un modo "giusto" o "sbagliato" per concatenare stringhe in Bash, quindi puoi sperimentare e trovare il metodo che funziona meglio per te.

## Vedi anche

- [Documentazione ufficiale di Bash](https://www.gnu.org/software/bash/)
- [Guida Bash per principianti](https://linuxize.com/post/bash-scripting-commands/)
- [Concatenazione di stringhe in altri linguaggi di programmazione](https://www.ionos.com/digitalguide/websites/web-development/concatenating-strings-in-javascript-php-and-python/)