---
title:                "Komennoriviparametrien lukeminen"
date:                  2024-01-20T17:55:24.291492-07:00
model:                 gpt-4-1106-preview
simple_title:         "Komennoriviparametrien lukeminen"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)
Komennorivin argumenttien lukeminen tarkoittaa parametrien vastaanottamista ohjelmaan komentokehotteesta. Se on hyödyllistä, koska voimme muokata ohjelman toimintaa lennosta käyttäjän syötteillä.

## How to: (Kuinka tehdä:)
```C
#include <stdio.h>

int main(int argc, char *argv[]) {
    printf("Ohjelmaan annettujen argumenttien lukumäärä: %d\n", argc);
    for (int i = 0; i < argc; i++) {
        printf("Argumentti %d: %s\n", i, argv[i]);
    }
    return 0;
}
```
Käynnistä ohjelma komentokehotteesta:
```bash
$ gcc ohjelma.c -o ohjelma
$ ./ohjelma testi1 testi2
Ohjelmaan annettujen argumenttien lukumäärä: 3
Argumentti 0: ./ohjelma
Argumentti 1: testi1
Argumentti 2: testi2
```

## Deep Dive (Sukellus syvälle):
C:n standardikirjasto on tarjonnut tapoja lukea komentorivin argumentteja kauan sitten. `argc` edustaa "argument count" ja `argv[]` on "argument vector", joka sisältää itse argumentit. Jokainen moderni käyttöjärjestelmä tukee tätä mekanismia. Vaihtoehtoisia tapoja on, kuten käyttää `getopt`-funktion perhettä monimutkaisemmissa skenaarioissa. Lisäyksissä ja poistoissa komentorivisyntaksin yhteydessä kannattaa olla huolellinen, sillä väärät argumentit voivat aiheuttaa virheitä tai odottamattomia toimintoja ohjelmassa.

## See Also (Katso myös):
- GNU 'getopt': https://www.gnu.org/software/libc/manual/html_node/Getopt.html
- C Standard Library documentation: https://en.cppreference.com/w/c/header
- Online C Compiler for testing code snippets: https://www.onlinegdb.com/online_c_compiler