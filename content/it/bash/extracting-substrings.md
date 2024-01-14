---
title:                "Bash: Estrazione di sottostringhe"
simple_title:         "Estrazione di sottostringhe"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

## Perché

Nel linguaggio di programmazione Bash, un'operazione comune è l'estrazione di sottostringhe da una stringa più grande. Ciò può essere utile in molte situazioni, ad esempio per il filtraggio o la manipolazione di dati, la creazione di voci di menu dinamiche o la formattazione di output.

## Come fare

L'estrazione di sottostringhe in Bash è possibile grazie all'utilizzo della tecnica di "slicing", che ci consente di estrarre una porzione specifica di una stringa utilizzando la sintassi seguente:

```
${stringa:inizio:lunghezza}
```

Dove "stringa" è la stringa originale, "inizio" è l'indice da cui iniziare l'estrazione e "lunghezza" è il numero di caratteri da estrarre. Se non viene specificata la lunghezza, la sottostringa verrà estratta fino alla fine della stringa originale.

Ad esempio, supponiamo di avere la seguente stringa:

```
frase="Questo è un esempio di stringa"
```

Per estrarre la parola "esempio", possiamo utilizzare il comando seguente:

```
${frase:11:7}
```

Questo ci restituirà "esempio", in quanto l'estrazione inizia dall'11° carattere e continua per 7 caratteri successivi.

Possiamo anche utilizzare numeri negativi per indicare l'inizio da cui estrarre la sottostringa dalla fine della stringa. Ad esempio, per estrarre l'ultima parola "stringa", possiamo utilizzare:

```
${frase: -7}
```

Questo ci restituirà "stringa", in quanto l'estrazione inizia da 7 caratteri prima della fine della stringa.

## Approfondimento

È possibile utilizzare anche altri argomenti nella sintassi di "slicing" per ottenere ulteriori risultati. Ad esempio, possiamo utilizzare ":- inizio" per estrarre i caratteri dalla stringa iniziale fino all'indice specificato; "inizio: -" per estrarre i caratteri dalla stringa dall'indice specificato fino alla fine; o ":-" per estrarre tutti i caratteri dalla stringa.

Inoltre, Bash ci consente di utilizzare l'operatore di "sostituzione dei parametri" "#" e "##" per rimuovere una porzione specifica dalla sottostringa. Ad esempio, possiamo utilizzare "#stringa" per rimuovere la stringa "stringa" all'inizio della sottostringa e "##stringa" per rimuoverla dall'inizio della stringa fino alla prima occorrenza della stringa specificata.

## Vedi anche

- [Documentazione Bash su "slicing"](https://www.shellscript.sh/tips/slice.html)
- [Utilizzo dei parametri di sostituzione in Bash](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html)