---
title:                "Concatenazione di stringhe"
html_title:           "Gleam: Concatenazione di stringhe"
simple_title:         "Concatenazione di stringhe"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

## Perché

Se sei nuovo a programmazione, potresti chiederti perché dovresti interessarti alla concatenazione di stringhe. In poche parole, la concatenazione di stringhe è un concetto fondamentale nella creazione di programmi per manipolare e visualizzare testo.

## Come fare

Per concatenare due stringhe usando Gleam, puoi utilizzare l'operatore ```++```, che unisce due stringhe in una sola. Ad esempio:

```Gleam
let nome = "Maria"
let cognome = "Rossi"
let nome_completo = nome ++ " " ++ cognome

io.println(nome_completo)

// Output: Maria Rossi
```

Se vuoi concatenare più di due stringhe, puoi utilizzare l'operatore all'interno di un'espressione, come nell'esempio seguente:

```Gleam
let nome = "Maria"
let cognome = "Rossi"
let saluto = "Ciao, "
let nome_completo = saluto ++ nome ++ " " ++ cognome ++ "!"

io.println(nome_completo)

// Output: Ciao, Maria Rossi!
```

## Approfondimento

La concatenazione di stringhe è una delle operazioni più comuni nella programmazione e viene utilizzata per costruire nuove stringhe combinando parti di testo esistente. Questo può essere utile, ad esempio, per creare messaggi personalizzati, per l'elaborazione di dati o per generare output.

Oltre all'operatore ```++```, Gleam offre anche altre funzioni utili per la manipolazione delle stringhe, come ```length```, che restituisce la lunghezza di una stringa, e ```slice```, che permette di estrarre una sottostringa da una stringa più grande.

Inoltre, è importante notare che la concatenazione di stringhe può avere un impatto sulle prestazioni del tuo programma, poiché ogni volta che si crea una nuova stringa, il sistema deve allocare nuova memoria per essa. Perciò, è importante utilizzare le funzioni stringa in modo accurato e ottimizzare il tuo codice quando possibile.

## Vedi anche

- [Documentazione di Gleam su stringhe](https://gleam.run/documentation/std-lib-string/)
- [Tutorial di Gleam per principianti](https://gleam.run/getting-started/basic-tutorial/)