---
title:                "Tekstitiedoston lukeminen"
html_title:           "Lua: Tekstitiedoston lukeminen"
simple_title:         "Tekstitiedoston lukeminen"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Tekstitiedoston lukeminen tarkoittaa tiedoston sisällön tarkastelemista ohjelmointikielen avulla. Ohjelmoijat tekevät niin tiedon käsittelyä, analysointia tai manipulointia varten.

## Miten tehdä:

Lukeminen tekstiedosto Bash:ssa on helppoa `cat`, `less` tai `more` komennon avulla.

```Bash
# Cat komento
cat tiedosto.txt

# Less komento
less tiedosto.txt

# More komento
more tiedosto.txt
```

Tuloste näyttää sisällön tiedostosta `tiedosto.txt`.

## Syvällisempi tieto:

*Bash* eli *Bourne-Again shell* on syntynyt alkujaan 1980-luvulla, ja se on otettu laajasti käyttöön sen laajan yhteensopivuuden ja käytettävyyden takia. Vaihtoehtoja Bashille ovat esimerkiksi *Korn shell* (ksh) ja *C shell* (csh). Tekstitiedostojen lukeminen Bashilla on melko yksinkertaista, mutta sen toteutuksessa on kuitenkin joitakin yksityiskohtia. Esimerkiksi `cat`, `less` ja `more` -komennot lukevat tiedoston sisällön eri tavoin. `Cat` tulostaa koko tiedoston sisällön kerralla, kun taas `less` ja `more` tulostavat sivun kerrallaan.

## Katso myös:

Seuraavasta hakuosumasta löytyy lisätietoa aiheesta ja aiheeseen liittyvistä asioista:
- GNU Bash official Documentation: [https://www.gnu.org/software/bash/manual/bash.html]
- Advanced Bash-Scripting Guide: [http://www.tldp.org/LDP/abs/html/abs-guide.html]
- Unix Shell Programming Books: [https://www.unix.com/answers-to-frequently-asked-questions/1373-books-on-shell-programming.html]