---
title:                "Lukemalla komentorivin argumentit."
html_title:           "PowerShell: Lukemalla komentorivin argumentit."
simple_title:         "Lukemalla komentorivin argumentit."
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Mitä ja miksi?

Komentoriviparametrien lukeminen on tapa saada tietoa erilaisista käyttäjän antamista syötteistä ohjelmalle. Tätä käytetään usein mukauttamaan ohjelman käyttäytymistä käyttäjän haluamalla tavalla.

# Miten:

Käytä seuraavaa koodia luodessasi PowerShell-skriptiä ja halutessasi lukea komentoriviparametrejä ja tulostaa ne:

```
PowerShell $args
```

Tässä esimerkissä tulostetaan kaikki komentoriviparametrit yhdellä kertaa. Voit myös valita tietyn parametrin antamalla sen nimen sulkuihin, kuten `$args[0]` ensimmäisestä parametrista. Tai voit käyttää `Read-Host`-komennon lukemaan yksittäisiä syötteitä käyttäjältä.

# Syvempi sukellus:

Komentoriviparametrit ovat olleet käytössä jo vuosia ja niitä käytetään monissa ohjelmointikielissä, kuten C, Java ja Python. Ne tarjoavat kätevän tavan antaa ohjelmalle tietoa suoraan käyttäjältä, ilman tarvetta muokata koodia joka kerta.

Toinen vaihtoehto komentoriviparametrien lukemiseen PowerShellissa on käyttää `Param()`-lohkoa skriptissä, mutta tämä vaatii hieman enemmän koodaustaitoja ja on paremmin soveltuvaa suurempiin projekteihin.

Komentoriviparametrit ovat myös käteviä ohjelman testaamisessa ja virheiden diagnosoinnissa. Jos jokin parametri aiheuttaa ongelmia, voit tarkistaa sen arvon helposti komentoriviltä.

# Katso myös:

Voit lukea lisää komentoriviparametreista ja niiden käytöstä PowerShellissa Microsoftin dokumentaatiosta: https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_parsing_command_line_parameters?view=powershell-7

Voit myös löytää erilaisia käyttökelpoisia vinkkejä ja esimerkkejä komentoriviparametrien lukemiseen Stack Overflow -sivustolta: https://stackoverflow.com/questions/15034573/powershell-args