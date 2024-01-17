---
title:                "Virhelähtöjen tulostaminen"
html_title:           "PowerShell: Virhelähtöjen tulostaminen"
simple_title:         "Virhelähtöjen tulostaminen"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Printtaaminen debug-outputin avulla tarkoittaa yksinkertaisesti ohjelmakoodin tulostamista näytölle. Tämä on hyödyllinen tapa tarkistaa ohjelman suorituksen aikana, että koodi toimii oikein ja havaita mahdolliset virheet. Ohjelmoijat käyttävät tätä menetelmää vianetsintään ja ohjelman kehittämiseen.

## Mitä ja miksi?

Printtaaminen debug-outputiksi tarkoittaa käytännössä ohjelmakoodin tulostamista ruudulle. Tämä on hyödyllinen tapa tarkistaa ohjelman toimintaa ja havaita mahdollisia virheitä sen suorittamisen aikana. Ohjelmoijat käyttävät tätä menetelmää vianetsintään ja ohjelmien parantamiseen.

## Kuinka:

```PowerShell

# Määritetään debug-outputin tulostusmuoto
$DebugPreference = "Continue"

# Printataan yksinkertainen viesti
Write-Debug "Ohjelma suoritetaan tällä hetkellä"

# Käytetään muuttujia debug-outputin tulostamiseen
$muuttuja = "Hello, World!"
Write-Debug "Tämä on muuttujan arvo: $muuttuja"

# Tuo esimerkki kompleksisemmasta koodista
if ($muuttuja -eq "Hello, World!") {
Write-Debug "Muuttuja sisältää halutun arvon"
}
else {
Write-Debug "Muuttuja sisältää jotain muuta"
}

```

Tulostus:

```
VERBOSE: Muuttuja sisältää halutun arvon.
```

Tämä esimerkki näyttää, että debug-outputin printtaaminen näytölle yksinkertaisten viestien tai muuttujien avulla on helppoa PowerShellillä. Tämä antaa ohjelmoijille tärkeää tietoa koodin suorituksen ajankohtana ja auttaa vianetsinnässä.

## Syväsukellus:

Debug-outputin käyttö on ollut tärkeä osa ohjelmointia jo vuosien ajan. Ennenkuin kehittäjien käytössä oli nykyaikaiset vianetsintätyökalut, debug-output oli usein ainoa tapa tarkistaa koodin toimivuutta. Nykyään on kuitenkin myös muita vianetsintämenetelmiä, kuten esimerkiksi erilaiset debugger-ohjelmat ja testausympäristöt.

PowerShell tarjoaa monipuolisia vaihtoehtoja debug-outputin hallintaan. Esimerkiksi komennon ```Set-PSDebug``` avulla voidaan säätää debug-outputin tasoja ja suodattaa halutut viestit. Lisäksi PowerShellin sisäänrakennetulla ```Write-Debug``` komennolla on mahdollista tulostaa debug-viestejä vain tietyissä tilanteissa, esimerkiksi vain tietyn muuttujan arvon ollessa haluttu.

## Katso myös:

Tutustu Microsoftin viralliseen dokumentointiin PowerShellin ```Write-Debug``` komennosta täällä: https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/write-debug

Lisää vinkkejä ja ohjeita debug-outputin käyttöön PowerShellillä löytyy täältä: https://www.red-gate.com/simple-talk/sysadmin/powershell/debugging-powershell-scripts-advanced-techniques/