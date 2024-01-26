---
title:                "Lavorare con i numeri complessi"
date:                  2024-01-26T04:37:02.945670-07:00
model:                 gpt-4-0125-preview
simple_title:         "Lavorare con i numeri complessi"
programming_language: "Bash"
category:             "Bash"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
I numeri complessi sono costituiti da una parte reale e una immaginaria. I programmatori li utilizzano in campi come l'elaborazione dei segnali, la meccanica quantistica e ogni volta che il calcolo lo richiede, poiché i normali numeri reali semplicemente non sono sufficienti.

## Come fare:
Bash non supporta nativamente i numeri complessi. Spesso si utilizza uno strumento esterno come `bc` con la sua opzione `-l`. Ecco come si elaborano i numeri complessi in bash:

```bash
echo "sqrt(-1)" | bc -l
```

Risultato:
```bash
j
```

Moltiplicazione:

```bash
echo "(-1 + -1i) * (4 + 3i)" | bc -l
```

Risultato:
```bash
-1.00000000000000000000-7.00000000000000000000i
```

## Approfondimento
I numeri complessi esistono dal XVI secolo, ma i linguaggi di scripting come Bash non sono predisposti per calcoli matematici complessi come i numeri complessi di base. Ecco perché strumenti come `bc` o altri come `awk` vengono spesso utilizzati. Alcuni linguaggi alternativi per lavorare con i numeri complessi sono Python con il suo modulo `cmath` e MATLAB, che sono entrambi progettati per funzioni matematiche più avanzate. Per quanto riguarda Bash, si tratta tutto di sfruttare gli strumenti - `bc` usa la 'i' minuscola per rappresentare l'unità immaginaria e supporta operazioni di base come l'addizione, la sottrazione, la moltiplicazione e la divisione.

## Vedi Anche
- Il manuale di `bc`: https://www.gnu.org/software/bc/manual/html_mono/bc.html
- GNU Octave (alternativa a MATLAB): https://www.gnu.org/software/octave/
- Modulo `cmath` di Python: https://docs.python.org/3/library/cmath.html