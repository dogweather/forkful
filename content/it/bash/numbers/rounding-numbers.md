---
date: 2024-01-26 03:43:09.766930-07:00
description: "Arrotondare i numeri significa tagliare le cifre decimali fino a un\
  \ valore pi\xF9 semplice che sia sufficientemente buono per un determinato contesto.\
  \ I\u2026"
lastmod: 2024-02-19 22:05:02.663251
model: gpt-4-0125-preview
summary: "Arrotondare i numeri significa tagliare le cifre decimali fino a un valore\
  \ pi\xF9 semplice che sia sufficientemente buono per un determinato contesto. I\u2026"
title: Arrotondamento dei numeri
---

{{< edit_this_page >}}

## Cosa e perché?

Arrotondare i numeri significa tagliare le cifre decimali fino a un valore più semplice che sia sufficientemente buono per un determinato contesto. I programmatori arrotondano i numeri per semplificare i risultati, risparmiare spazio o perché il valore esatto non è fondamentale, come quando si sta valutando a occhio l'uso della CPU o lo spazio su disco e le cifre decimali non faranno la differenza nella tua giornata.

## Come fare:

Ecco l'abc sull'arrotondamento in Bash:

```Bash
# Arrotondare per difetto usando 'floor' con bc
echo "scale=0; 3.49/1" | bc

# Arrotondare per eccesso usando 'ceiling' con bc
echo "scale=0; 3.01/1" | bc -l

# Arrotondare al più vicino intero usando printf
printf "%.0f\n" 3.49

# Un trucco per arrotondare al più vicino intero usando bc
echo "(3.49+0.5)/1" | bc
```

Esempi di output—direttamente dalla bocca del terminale:

```
3  # Arrotondato per difetto (floor)
4  # Arrotondato per eccesso (ceiling)
3  # Arrotondato al più vicino (con printf)
3  # Arrotondato al più vicino (con bc)
```

## Approfondimento

Ai vecchi tempi, non c'erano `bc` o `printf` negli script Bash per fare magie matematiche. Gli old-school dovevano affidarsi a strumenti esterni o a soluzioni ingegnose di compromesso. Ora, `bc` ti consente di fare matematica di precisione. Tieni presente, `bc` non arrotonda per impostazione predefinita—fa il floor. La parte della scala imposta l'azione del punto decimale.

Alternative? Potresti usare `awk` per l'arrotondamento senza passare a `bc` o cimentarti con `perl` per esigenze matematiche più pesanti. Per i masochisti, va puro Bash con, diciamo, la manipolazione iterativa delle stringhe – ma perché?

Per quanto riguarda i dettagli, `bc` non si limita ad arrotondare, fa un mucchio di roba matematica—scala, seno, radice quadrata, qualsiasi cosa. Con `printf`, si tratta più di formattare il testo, ma ei, arrotonda i numeri, quindi non ci lamentiamo.

## Vedi Anche

Per coloro che vogliono saperne di più:

- Manuale GNU `bc`: https://www.gnu.org/software/bc/manual/html_mono/bc.html
- Comando Bash `printf`: https://www.gnu.org/software/bash/manual/html_node/Bash-Builtins.html#index-printf
- Guida per l'utente di AWK (per l'arrotondamento e altre elaborazioni di testo): https://www.gnu.org/software/gawk/manual/gawk.html
- Altre matematiche, script e trucchi numerici in Bash: https://mywiki.wooledge.org/BashFAQ/022
