---
title:                "Tarkistetaan, onko hakemistoa olemassa"
html_title:           "PowerShell: Tarkistetaan, onko hakemistoa olemassa"
simple_title:         "Tarkistetaan, onko hakemistoa olemassa"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Mitä ja miksi?
Tarkistaminen, onko hakemisto olemassa, on yksinkertainen mutta tärkeä askel PowerShell-ohjelmoijille. Se mahdollistaa tiedostojen käyttämisen ja käsittelyn oikeassa paikassa, jolloin koodi toimii sujuvammin.

# Miten:
```PowerShell
# Tarkistetaan, onko hakemisto olemassa
Test-Path C:\Users\Käyttäjä\Asiakirjat

# Tulos: 
True
```

```PowerShell
# Esimerkki, jossa hakemisto ei ole olemassa
Test-Path C:\Users\Käyttäjä\Kuvat

#Tulos:
False
```

# Syvempi sukellus:
- Historiallinen konteksti: Tarkistaminen, onko hakemisto olemassa, on ollut osa Unix-käyttöjärjestelmää jo vuosikymmeniä. PowerShellissä se mahdollistaa joustavan koodin kirjoittamisen tiedostojen käsittelyyn.
- Vaihtoehtoiset tavat: Lisäksi Test-Path-komento, on myös mahdollista käyttää [Get-ChildItem](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/get-childitem?view=powershell-7.1) tai [dir](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/dir?view=powershell-7.1) komentoja hakemiston tarkistamiseen.
- Toteutus: Test-Path-komento perustuu [Provider](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_providers?view=powershell-7.1)-periaatteeseen, joka mahdollistaa tiedostojen ja hakemistojen käsittelemisen yhtenäisellä tavalla.

# Katso myös:
- [About_Providers](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_providers?view=powershell-7.1)
- [Get-ChildItem](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/get-childitem?view=powershell-7.1)
- [dir](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/dir?view=powershell-7.1)