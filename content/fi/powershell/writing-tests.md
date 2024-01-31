---
title:                "Testien kirjoittaminen"
date:                  2024-01-19
html_title:           "Arduino: Testien kirjoittaminen"
simple_title:         "Testien kirjoittaminen"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? - Mikä & Miksi?
Testaus tarkoittaa koodisi varmistamista: tekeekö se mitä pitää. Testaamalla säilytät mielenrauhan ja säästät aikaa, kun bugit löytyvät nopeasti.

## How to: - Kuinka Tehdään:
Testien kirjoittaminen PowerShellissa käyttämällä Pester-testikehystä:

```PowerShell
# Asenna Pester-moduuli, jos sitä ei vielä ole
Install-Module -Name Pester -Scope CurrentUser -Force -SkipPublisherCheck

# Lataa Pester
Import-Module Pester

# Luo yksinkertainen funktio, jota testataan
function Get-MultiplyResult($a, $b) {
    return $a * $b
}

# Kirjoita testit
Describe "Get-MultiplyResult Tests" {
    It "Kertoo luvut oikein" {
        Get-MultiplyResult 7 3 | Should -Be 21
    }
    It "Ei palauta virheellistä tulosta" {
        Get-MultiplyResult 2 2 | Should -Not -Be 5
    }
}

# Suorita testit
Invoke-Pester
```

Testien tulokset näyttävät seuraavalta:

```
Describing Get-MultiplyResult Tests
 [+] Kertoo luvut oikein 40ms
 [+] Ei palauta virheellistä tulosta 5ms
```

## Deep Dive - Syväsukellus:
Testaaminen PowerShellissä ei ole aina ollut yhtä helppoa. Pester-testikehys ilmestyi vuonna 2015 ja muutti pelin. Vaihtoehtoja on, esimerkiksi PSUnit, mutta Pester on suosituin ja PowerShellin yhteisön eniten hyväksymä. Testeissä käytetään BDD-tyylisiä "Describe" ja "It" lohkoja, mikä tekee niistä luettavia ja ylläpidettäviä.

## See Also - Katso Myös:
- Pester-projektin GitHub-sivu: https://github.com/pester/Pester
- PowerShellin testaamisen parhaat käytännöt: https://docs.microsoft.com/en-us/powershell/scripting/dev-cross-plat/writing-portable-modules?view=powershell-7.1
- Mikä on BDD (Behavior-Driven Development): https://en.wikipedia.org/wiki/Behavior-driven_development
