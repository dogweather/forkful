---
title:                "Tiedoston kirjoittaminen"
html_title:           "PowerShell: Tiedoston kirjoittaminen"
simple_title:         "Tiedoston kirjoittaminen"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?

Tekstitiedoston kirjoittaminen tarkoittaa yksinkertaisesti tiedoston sisällön tallentamista tekstimuodossa. Ohjelmoijat tekevät sitä usein tallentaakseen ja muokatakseen tietoja sekä luodakseen ja hallitakseen sisältöä eri ohjelmien välillä.

## Ohjeet:

Käytä alla olevia esimerkkejä kirjoittaaksesi uuden tekstitiedoston ja tallentaaksesi sisällön muuttujaan:

```PowerShell
# Luo uusi tekstitiedosto
New-Item -Path "C:\esimerkkitiedosto.txt" -ItemType File

# Tallenna sisältö tiedostoon
Set-Content -Path "C:\esimerkkitiedosto.txt" -Value "Tämä on esimerkkitiedosto."
```

Voit myös lisätä sisältöä olemassa olevaan tekstitiedostoon käyttämällä `Add-Content` -komentoa:

```PowerShell
# Lisää sisältöä olemassa olevaan tekstitiedostoon
Add-Content -Path "C:\esimerkkitiedosto.txt" -Value "Tämä on lisää esimerkkisisältöä."
```

Lopuksi, voit lukea tiedoston sisällön käyttämällä `Get-Content` -komentoa:

```PowerShell
# Lue tiedoston sisältö
Get-Content -Path "C:\esimerkkitiedosto.txt"

# Tuottaa seuraavan tulosteen:
# Tämä on esimerkkitiedosto.
# Tämä on lisää esimerkkisisältöä.
```

## Syvällinen sukellus:

Tekstitiedostojen kirjoittaminen on ollut oleellinen osa ohjelmointia jo vuosien ajan, ja se on edelleen tärkeä osa monien työtehtävien suorittamista. Ohjelmoijat voivat myös käyttää muita rekisteri- tai tietokantamuotoja tallentaakseen ja hallitakseen tietoja, mutta tekstitiedostot ovat nopeita ja yksinkertaisia vaihtoehtoja pienemmille tiedoille.

## Katso myös:

Tutustu seuraaviin lähteisiin saadaksesi lisätietoja tekstitiedostojen kirjoittamisesta PowerShellilla:

- [PowerShell-asiakirja](https://docs.microsoft.com/fi-fi/powershell/scripting/overview) – Lisätietoja PowerShell-skriptikielestä ja sen käytöstä.
- [Tekstitiedoston kirjoittaminen](https://www.tutorialspoint.com/powershell/powershell_writing_files.htm) – Hyödyllinen opas tekstitiedoston kirjoittamisesta PowerShellilla vaihe vaiheelta.
- [PowerShell-skriptiesimerkkejä](https://github.com/PowerShell/PowerShell/tree/master/examples) – Monipuolinen valikoima valmiita PowerShell-skriptejä, joihin kuuluu myös tekstitiedostojen kirjoittamiseen liittyviä esimerkkejä.