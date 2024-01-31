---
title:                "Tiedostojen muokkaaminen paikan päällä komentorivin yhden rivin komennoilla"
date:                  2024-01-27T16:20:56.096811-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tiedostojen muokkaaminen paikan päällä komentorivin yhden rivin komennoilla"

tag:                  "Data and Text Processing"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/editing-files-in-place-with-cli-one-liners.md"
---

{{< edit_this_page >}}

## Mikä ja miksi?

Tiedostojen paikan päällä muokkaaminen CLI-yhden komentorivin avulla PowerShellissä tarkoittaa suorien muutosten tekemistä tiedostoihin komentoriviltä ilman, että niitä tarvitsee avata editorissa. Tämä lähestymistapa säästää aikaa ja on erityisen kätevää erissä tapahtuvassa käsittelyssä tai toistuvien muokkaustehtävien automatisoinnissa useissa tiedostoissa.

## Kuinka tehdä:

### Tekstin korvaaminen yhdessä tiedostossa

Aloitetaan yksinkertaisella tehtävällä: haluat korvata kaikki "oldtext"-esiintymät "newtext"-tekstillä tiedostossa nimeltä example.txt. Näin teet sen:

```PowerShell
(Get-Content example.txt) -replace 'oldtext', 'newtext' | Set-Content example.txt
```

Tämä yhden rivin komento lukee sisällön, suorittaa korvauksen ja kirjoittaa sisällön takaisin alkuperäiseen tiedostoon.

### Useiden tiedostojen muokkaaminen

Entä jos tarvitset tehdä saman muutoksen useissa tiedostoissa? Tässä yksi lähestymistapa käyttäen silmukkaa:

```PowerShell
Get-ChildItem *.txt | ForEach-Object {
  (Get-Content $_) -replace 'oldtext', 'newtext' | Set-Content $_
}
```

Tämä pätkä etsii kaikki `.txt`-tiedostot nykyisestä hakemistosta ja korvaa "oldtext"-tekstin "newtext"-tekstillä jokaisessa.

### Sisällön lisääminen tiedostojen alkuun tai loppuun

Sisällön liittäminen alkuun tai loppuun voidaan myös virtaviivaistaa:

```PowerShell
# Liittäminen alkuun
"Uusi ensimmäinen rivi`n" + (Get-Content example.txt) | Set-Content example.txt

# Liittäminen loppuun
(Get-Content example.txt) + "`nUusi viimeinen rivi" | Set-Content example.txt
```

Tässä yksinkertaisesti yhdistämme uuden sisällön ennen tai jälkeen nykyisen sisällön ja tallennamme sen takaisin.

## Syväsukellus

Historiallisesti paikan päällä tehtävät muokkaukset ovat enemmän yhdistetty Unix-työkaluihin kuten `sed` ja `awk`. PowerShell, ollessaan suhteellisen uusi tulokas, ei sisällä valmiina erillistä paikan päällä muokkaamisen ominaisuutta. Tämä johtuu osittain sen suunnittelufilosofiasta, joka korostaa objektien tärkeyttä tekstivirtojen sijaan, toisin kuin Unix-työkalut, jotka käsittelevät useimpia syötteitä tekstiksi.

Vaihtoehtoja PowerShellille tähän tehtävään sisältävät perinteiset Unix-työkalut, jotka ovat saatavilla Windowsille Cygwinin tai Windowsin alijärjestelmän Linuxille (WSL) kautta. Nämä työkalut tarjoavat usein tiiviimmän syntaksin paikan päällä tehtävään muokkaukseen niiden tekstikeskeisen suunnittelun ansiosta.

Toteutuksen kannalta on tärkeää huomata, että PowerShellin lähestymistapa sisältää koko tiedoston lukemisen muistiin, muutosten tekemisen ja sitten sen kirjoittamisen takaisin. Vaikka tämä toimii hyvin kohtuullisen kokoisille tiedostoille, se voi muuttua tehottomaksi hyvin suurille tiedostoille. Tällaisissa tapauksissa saatetaan harkita `.NET`-menetelmien suoraa käyttöä tai vaihtoehtoisten työkalujen käyttöä, jotka on suunniteltu suurten datamäärien suoratoistoon.

Huolimatta näistä seikoista, PowerShellin joustavuus ja laaja ominaisuusvalikoima tekevät siitä arvokkaan työkalun tiedostojen suoraan käsittelyyn komentoriviltä, erityisesti niille, jotka ovat jo syvällä Windows-ekosysteemissä tai hallinnoivat alustojen välisiä ympäristöjä.
