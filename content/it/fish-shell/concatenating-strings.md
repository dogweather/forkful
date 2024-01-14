---
title:                "Fish Shell: Unione di stringhe"
simple_title:         "Unione di stringhe"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Perché
Concatenare stringhe è una pratica comune nella programmazione che può aiutare a creare stringhe più lunghe e complesse a partire da più stringhe più corte. E' una funzionalità utile da utilizzare quando si vuole creare un output dinamico o quando si lavora con database.

## Come fare
Per concatenare stringhe in Fish Shell, è possibile utilizzare l'operatore di concatenazione "++". Ad esempio, se si volesse creare una stringa che contenesse il nome e il cognome, si potrebbe scrivere:

```Fish Shell
set first_name "Mario"
set last_name "Rossi"
echo $first_name ++ $last_name
```

Questo codice produrrebbe in output "MarioRossi". E' importante notare che l'operatore di concatenazione non aggiunge spazi tra le stringhe, quindi è necessario aggiungerli manualmente, se necessario.

Un'altra opzione per concatenare stringhe è utilizzare la funzione "string join", che consente di unire diverse stringhe con un separatore specifico. Ad esempio:

```Fish Shell
set fruits "apple" "banana" "orange"
string join ", " $fruits
```

Questo codice produrrebbe in output "apple, banana, orange".

## Approfondimento
Oltre a "++" e "string join", è possibile concatenare le stringhe utilizzando la variabile "$argv", che rappresenta gli argomenti passati in input alla shell. Utilizzando questo approccio, è possibile creare una stringa personalizzata in base ai valori passati in input. Ad esempio:

```Fish Shell
set nome $argv[1]
set cognome $argv[2]
echo "Benvenuto $nome $cognome!"
```

Passando in input "Marco Rossi", l'output sarebbe "Benvenuto Marco Rossi!".

## Vedi anche
- [Fish Shell documentazione su concatenazione di stringhe](https://fishshell.com/docs/current/tutorial.html#tut_variable_expansion)
- [Esempi di concatenazione di stringhe in Fish Shell](https://github.com/fish-shell/fish-shell/wiki/FAQ#how-can-i-concatenate-strings)
- [Tutorial introduttivo su Fish Shell](https://youtu.be/lhS7YCijTM0) (in italiano)