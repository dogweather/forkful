---
date: 2024-01-26 03:39:12.564927-07:00
description: "Miten: Fishiss\xE4 on sis\xE4\xE4nrakennettua magiaa t\xE4llaiseen teht\xE4\
  v\xE4\xE4n. K\xE4yt\xE4 `string`-funktiota hikoilematta. Tutustu n\xE4ihin loitsuihin."
lastmod: '2024-03-13T22:44:56.980207-06:00'
model: gpt-4-0125-preview
summary: "Fishiss\xE4 on sis\xE4\xE4nrakennettua magiaa t\xE4llaiseen teht\xE4v\xE4\
  \xE4n."
title: Merkkijonosta lainausmerkkien poistaminen
weight: 9
---

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
