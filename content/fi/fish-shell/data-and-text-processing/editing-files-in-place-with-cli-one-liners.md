---
aliases:
- /fi/fish-shell/editing-files-in-place-with-cli-one-liners/
date: 2024-01-27 16:21:19.895288-07:00
description: "Tiedostojen suoraan muokkaaminen CLI-yksirivikomennoin tarkoittaa muutosten\
  \ tekemist\xE4 suoraan tiedostoihin komentorivilt\xE4, ilman niiden avaamista\u2026"
lastmod: 2024-02-18 23:09:08.079358
model: gpt-4-0125-preview
summary: "Tiedostojen suoraan muokkaaminen CLI-yksirivikomennoin tarkoittaa muutosten\
  \ tekemist\xE4 suoraan tiedostoihin komentorivilt\xE4, ilman niiden avaamista\u2026"
title: "Tiedostojen muokkaaminen paikan p\xE4\xE4ll\xE4 komentorivin yhden rivin komennoilla"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Tiedostojen suoraan muokkaaminen CLI-yksirivikomennoin tarkoittaa muutosten tekemistä suoraan tiedostoihin komentoriviltä, ilman niiden avaamista tekstieditorissa. Ohjelmoijat tekevät näin säästääkseen aikaa ja automatisoidakseen toistuvia muokkaustehtäviä, tehden työnkulustaan sujuvamman ja tehokkaamman.

## Kuinka:

Fish Shell, tunnettu käyttäjäystävällisistä ominaisuuksistaan ja tehokkaista skriptausmahdollisuuksista, tarjoaa useita tapoja muokata tiedostoja suoraan. Toisin kuin jotkut muut shellit, Fishillä ei kuitenkaan ole sisäänrakennettua mekanismia suoraan muokkaamiseen (`sed -i` Bashissa esimerkiksi). Mutta älä pelkää, voit silti saavuttaa tämän pienellä luovuudella ja apua ulkoisilta työkaluilta kuten `sed` ja `awk`.

### `sed`-komennon käyttö yksinkertaisiin korvauksiin
Korvataksesi kaikki "hello"-esiintymät sanalla "world" tiedostossa `file.txt`, käyttäisit:
```Fish Shell
sed -i '' 's/hello/world/g' file.txt
```

### Useiden `sed`-komentojen soveltaminen
Jos tarvitset suorittaa useita korvauksia, voit ketjuttaa ne näin:
```Fish Shell
sed -i '' -e 's/fish/bass/g' -e 's/rainbow/trout/g' file.txt
```

### `awk`-komennon käyttö monimutkaisempiin toimintoihin
Toimintoihin, jotka ovat liian monimutkaisia `sed`-komentoa varten, `awk` voi olla työkalusi. Näin voit kaksinkertaistaa jokaisen rivin numeron:
```Fish Shell
awk '{print $1 * 2}' file.txt > temp && mv temp file.txt
```

### Huomio virheenkäsittelystä
Muista, että näitä työkaluja käytettäessä Fishissä, virheiden kaappaaminen ja niiden viestien ymmärtäminen on olennaisen tärkeää. Käytä Fishin vankkaa virheenkäsittelyä tehdäksesi skriptisi luotettavammiksi.

## Syväsukellus

Historiallisesti suoraan tiedoston muokkaaminen on ollut Unixin ja Linuxin ohjelmoinnin kulmakivi, tarjoten tehokkaan tavan suorittaa nopeita muokkauksia manuaalisesti avaamatta tiedostoja. Työkalut kuten `sed` ja `awk` ovat arvostettuja apuohjelmia, jotka ovat olleet olemassa Unixin alkupäivistä lähtien, tullen välttämättömiksi tekstinkäsittelytehtävissä.

Fish Shell, ollessaan modernimpi ja tarjoten parannuksia käytettävyydessä ja skriptauksessa, puuttuu sisäänrakennettu suora muokkaus pääasiassa suunnittelufilosofiansa vuoksi, joka keskittyy vuorovaikutteisuuteen ja käyttäjäystävällisyyteen. Fishissä ei ole omaa suoraan muokkauskomentoa, mikä korostaa ulkoisten työkalujen tärkeyttä Unix-kaltaisissa ekosysteemeissä.

Vaihtoehtoja suoraan muokkaukseen Fishissä ovat väliaikaisten tiedostojen käyttö tai Perl- tai Python-yksirivikomentojen hyödyntäminen, jotka voivat tarjota enemmän joustavuutta tai luettavuutta monimutkaisissa tehtävissä.

Esimerkiksi käyttäen Perliä:
```Fish Shell
perl -pi -e 's/find/replace/g' file.txt
```
Tai Pythonia:
```Fish Shell
python -c "import re, sys; [sys.stdout.write(re.sub('pattern', 'replacement', line)) for line in sys.stdin]" < file.txt > temp && mv temp file.txt
```

Toteutuksen kannalta, kun suoritat suoran muokkauksen, nämä työkalut luovat tyypillisesti väliaikaisen tiedoston, kirjoittavat muutokset sinne ja korvaavat sitten alkuperäisen tiedoston muokatulla versiolla. Tämä lähestymistapa varmistaa, että tiedoston muokkausprosessi ei vahingoita tai menetä tietoja, jos operaation aikana tapahtuu virhe.

Näiden työkalujen ja menetelmien ymmärtäminen mahdollistaa Fish Shell -ohjelmoijien tehokkaan sisällyttämisen suoraan muokkaukseen skripteihinsä, yhdistäen Fishin käyttäjäystävälliset ominaisuudet perinteisen Unix-tekstinkäsittelyn raakaan voimaan.
