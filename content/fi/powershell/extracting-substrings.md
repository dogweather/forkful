---
title:                "Alimerkkijonojen poiminta"
html_title:           "Gleam: Alimerkkijonojen poiminta"
simple_title:         "Alimerkkijonojen poiminta"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/extracting-substrings.md"
---

{{< edit_this_page >}}

# PowerShell Alimerkkijonojen Käyttö

## Mitä & Miksi?

Alimerkkijonot ovat osa isompaa merkkijonoa. Ohjelmoijat käyttävät alimerkkijonoja kun he haluavat erottaa tietyt pätkät datasta tai muotoilla tekstiä tietyllä tavalla.

## Miten Tehdään:

Käytetään joko `substring` tai `remove` metodeja PowerShellissä. Tässä on esimerkkejä:

```PowerShell
# Luodaan merkkijono
$s = "Tervetuloa PowerShellin maailmaan!"

# Otetaan alimerkkijono käyttämällä substring-metodia
$subs = $s.substring(0,9)
$subs

# Tulostaa: "Tervetulo"
```

```PowerShell
# Luodaan merkkijono
$s = "Tervetuloa PowerShellin maailmaan!"

# Otetaan alimerkkijono käyttämällä remove-metodia
$subs = $s.Remove(9)
$subs

# Tulostaa: "Tervetulo"
```

## Syvemmälle:

`Substring` tekee leikkauksen määritellystä kohdasta merkkijonossa. Tämä metodi on ollut olemassa pitkään monissa ohjelmointikielissä, mukaan lukien PowerShell.

`Remove` poistaa määritellyn kohdan loppuun asti. Tämä on toinen tapa tehdä sama asia, mutta se sopii paremmin silloin, kun halutaan poistaa osa merkkijonosta.

Käyttäjä voi valita, kumpaa metodia käyttää tarpeensa mukaisesti. Molemmat tukevat PowerShellin kokonaan, mukaan lukien erilaiset kulttuuriasetukset, joten kumman tahansa valinnassa ei ole suurta eroa.

## Katso Myös:

* Microsoftin dokumentaatiosta löytyy lisää tietoa [`substring`](https://docs.microsoft.com/fi-fi/dotnet/api/system.string.substring?view=net-5.0) ja [`remove`](https://docs.microsoft.com/fi-fi/dotnet/api/system.string.remove?view=net-5.0) metodeista.
* [Stack Overflow](https://stackoverflow.com/questions/14104547/what-is-the-best-way-to-extract-a-substring-in-powershell) on hyvä lähde nähdä lisää esimerkkejä ja keskusteluja alimerkkijonojen käytöstä PowerShellissä.