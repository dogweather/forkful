---
title:                "Debuggerin käyttö"
date:                  2024-01-26T03:47:50.619414-07:00
model:                 gpt-4-0125-preview
simple_title:         "Debuggerin käyttö"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/using-a-debugger.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Debuggeri on työkalu, jonka avulla voit tutkia C-koodisi toimintaa sen suorituksen aikana, askel askeleelta, löytääksesi ohjelmointivirheitä. Ohjelmoijat käyttävät debuggeria ymmärtääkseen, miten heidän koodinsa käyttäytyy, korjatakseen ongelmia ja optimoidakseen suorituskykyä arvauspelin sijaan.

## Kuinka:
Oletetaan, että työskentelet yksinkertaisen C-ohjelman parissa, joka laskee luvun kertoman, mutta esiintyy virhe. Debuggerin, kuten `gdb` (GNU Debugger), käyttämiseksi käännä ensin `-g`-lipulla sisällyttääksesi debug-tiedot:

```c
// käännä käyttäen: gcc factorial.c -o factorial -g
#include <stdio.h>

long factorial(int n) {
    if (n < 0) return 0; // Yksinkertainen tarkistus negatiiviselle syötteelle
    long tulos = 1;
    while (n > 1)
        tulos *= n--;
    return tulos;
}

int main() {
    int numero = 5;
    long tulos = factorial(numero);
    printf("Luvun %d kertoma on %ld\n", numero, tulos);
    return 0;
}
```

Aja sitten se gdb:ssä:

```shell
$ gdb ./factorial
```

Aseta katkaisupiste `factorial`-funktioon ja aja ohjelma:

```gdb
(gdb) break factorial
(gdb) run
```

Kun se saavuttaa katkaisupisteen, etene jokaisen rivin läpi käyttäen `next` tai `n` ja tarkastele muuttujia käyttäen `print` tai `p`:

```gdb
(gdb) next
(gdb) print tulos
$1 = 1
```

Esimerkkivastaus tarjoaa reaaliaikaisia arvoja ja ohjelman suoritusvirtaa.

## Syväsukellus
Debuggerit ovat olleet olemassa 1960-luvulta lähtien, kehittyen yksinkertaisista monitoroinneista monimutkaisiin, GUI-pohjaisiin sovelluksiin. Vanhanaikainen tulosteisiin perustuva debuggaus oli yleistä ennen kypsien debuggerien kehittämistä. Vaihtoehtoja `gdb`:lle ovat `lldb`, `dbx`, tai IDE-integroidut debuggerit, kuten ne Visual Studio- tai CLion-ohjelmoissa.

Debuggereiden kanssa työskentelyssä toteutus vaihtelee—joissakin voi havaita suoritusaikaisia virheitä, tutkia muistia tai jopa kääntää ohjelman suoritusta. `gdb` voi liittyä käynnissä oleviin prosesseihin, mahdollistaen jo käynnissä olevan ohjelmiston debuggauksen, mikä on etu live-järjestelmävirheiden korjaamisessa.

## Katso Myös
- GNU Debugger (GDB): https://www.gnu.org/software/gdb/documentation/
- Debugging with GDB: https://sourceware.org/gdb/current/onlinedocs/gdb
- LLDB Debugger: https://lldb.llvm.org/use/tutorial.html
- Debugging Techniques in C: http://www.cprogramming.com/debugging/debugging.html