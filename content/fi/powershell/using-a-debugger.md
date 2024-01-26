---
title:                "Debuggerin käyttö"
date:                  2024-01-26T04:09:01.050958-07:00
model:                 gpt-4-0125-preview
simple_title:         "Debuggerin käyttö"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/using-a-debugger.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Debuggerin käyttö tarkoittaa katkaisukohtien asettamista, koodin läpikäyntiä askel askeleelta, muuttujien tarkkailua ja ohjelman tilan tarkastelua sen suorituksen aikana. Se on ohjelmoijille merkittävä muutos, koska se paikantaa bugeja ja auttaa meitä ymmärtämään, mitä koodimme todella tekee.

## Kuinka:
PowerShellissa voit debugata skriptejä käyttämällä sisäänrakennettua PowerShell Integrated Scripting Environment (ISE) -ympäristöä tai Visual Studio Code (VS Code) -ohjelmaa PowerShell-laajennuksen kanssa. Näin käytät katkaisukohtia molemmissa:

### PowerShell ISE:
```PowerShell
# Aseta katkaisukohta tietylle riville
Set-PSBreakpoint -Script .\MyScript.ps1 -Line 5

# Suorita skriptisi normaalisti
.\MyScript.ps1

# Kun skripti osuu katkaisukohtaan, voit tarkastella muuttujia
$myVariable

# Jatka suoritusta
Continue
```

### Visual Studio Code:
```PowerShell
# Avaa PowerShell-skriptisi VS Codessa.
# Napsauta rivinumeron vasemmalla puolella asettaaksesi katkaisukohdan.
# Aloita debuggaus painamalla F5 tai napsauttamalla 'Aloita debuggaus'.

# VS Code pysäyttää suorituksen katkaisukohtaan.
# Käytä debug-paneelia muuttujien tarkkailuun, kutsupinon tarkasteluun ja suorituksen hallintaan.
```

Debuggaus molemmissa ympäristöissä mahdollistaa sisään astumisen (F11), yli hyppäämisen (F10) ja ulos astumisen (Shift+F11) debuggauksen aikana.

## Syvempi sukellus
Historiallisesti PowerShellin debuggaus oli hieman kömpelöä; se vaati paljon `Write-Host`-rivilauseita muuttujien tilojen tulostamiseen tai klassista kokeilu- ja erehdysmenetelmää. PowerShell ISE:n myötä, ja viimeaikaisemmin, VS Coden rikkaiden debuggausominaisuuksien myötä, PowerShellin debuggaus on tullut lähes yhtä intuitiiviseksi kuin täysiverisissä ohjelmointikielissä.

Vaihtoehtoja PowerShellin natiiveille debuggaustyökaluille sisältävät kolmannen osapuolen työkalut, kuten PowerGUI, tai robustien IDE:iden, kuten Visual Studio, käyttö PowerShell-laajennuksen kanssa.

Debuggerin käyttöönotossa harkitse skriptin aluetta, erityisesti työskennellessäsi pistelähde-skriptien tai moduulien kanssa. Katkaisukohdat voivat olla ehtoperusteisia, muuttujan muutokseen perustuvia tai riviperusteisia, mahdollistaen tarkan hallinnan debuggausistunnon aikana.

Lisäksi siirtyessä PowerShell Coreen (alustojen välinen PowerShell), debuggaus on pääosin siirtynyt VS Coden käsiin, joka tarjoaa johdonmukaisen kokemuksen eri alustoilla.

## Katso myös
Lisätietoa PowerShellin debuggauksesta:
- [about_Debuggers](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_Debuggers)
- [PowerShellin dokumentaatio debuggauksesta](https://docs.microsoft.com/en-us/powershell/scripting/debugging/debugging-in-powershell?view=powershell-7.2)
- [Visual Studio Code PowerShell-laajennus](https://marketplace.visualstudio.com/items?itemName=ms-vscode.PowerShell)