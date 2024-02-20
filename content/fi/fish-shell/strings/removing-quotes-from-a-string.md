---
date: 2024-01-26 03:39:12.564927-07:00
description: "Merkkijonosta lainausmerkkien poistaminen tarkoittaa n\xE4iden kiusallisten\
  \ yksitt\xE4isten (' ') tai kaksinkertaisten (\" \") lainausmerkkien poistamista\u2026"
lastmod: 2024-02-19 22:05:15.874885
model: gpt-4-0125-preview
summary: "Merkkijonosta lainausmerkkien poistaminen tarkoittaa n\xE4iden kiusallisten\
  \ yksitt\xE4isten (' ') tai kaksinkertaisten (\" \") lainausmerkkien poistamista\u2026"
title: Merkkijonosta lainausmerkkien poistaminen
---

{{< edit_this_page >}}

## Mikä & Miksi?

Merkkijonosta lainausmerkkien poistaminen tarkoittaa näiden kiusallisten yksittäisten (' ') tai kaksinkertaisten (" ") lainausmerkkien poistamista tekstidatastasi. Ohjelmoijat tekevät tämän usein syötteen puhdistamiseksi tai datan valmistamiseksi edelleen käsiteltäväksi ilman lainausmerkkien sekasortoa.

## Miten:

Fishissä on sisäänrakennettua magiaa tällaiseen tehtävään. Käytä `string`-funktiota hikoilematta. Tutustu näihin loitsuihin:

```fish
# Esimerkki yksittäisillä lainausmerkeillä
set quoted "'Hello, World!'"
set unquoted (string trim --chars \"\'\" $quoted)
echo $unquoted # Tuloste: Hello, World!

# Sama juttu kaksinkertaisilla lainausmerkeillä
set double_quoted "\"Hello, Universe!\""
set unquoted (string trim --chars \"\'\" $double_quoted)
echo $unquoted # Tuloste: Hello, Universe!
```

## Syväsukellus

Komentorivin kivikaudella joutuisit painimaan `sed`- tai `awk`-komennon kanssa lainausmerkkien poistamiseksi; todellinen takkujen ja kryptisten lippujen sotku. Fishin `string`-funktio on uudemmalta ajalta, tekee koodista puhtaampaa ja intuitiivisempaa.

Muiden kuorien vaihtoehdot saattavat edelleen luottaa näihin vanhoihin työkaluihin tai saattavat käyttää omia sisäänrakennettuja metodejaan, kuten bashin parametrilaajennus tai zsh:n muokkaimet.

`String`-funktio menee lainausmerkkien karsimista pidemmälle. Se on Sveitsin armeijan linkkuveitsi merkkijonojen operaatioihin Fishissä. `String`-avulla voit viipaloida, kuutioida, jakaa, yhdistää tai jopa regex-vastata merkkijonoja suoraan terminaalissasi.

## Katso Myös

Sukella syvemmälle `string`-funktion maailmaan virallisen dokumentaation avulla:
- [Fish Shell String Dokumentaatio](https://fishshell.com/docs/current/commands.html#string)

Nostalgian vuoksi tai skriptatessasi perinteisempien kuorien kanssa, tutustu:
- [Sed & Awk Opas](https://www.grymoire.com/Unix/Sed.html)
- [Bashin Parametrilaajennus](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html)
