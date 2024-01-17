---
title:                "Työskentely jsonin kanssa"
html_title:           "PowerShell: Työskentely jsonin kanssa"
simple_title:         "Työskentely jsonin kanssa"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/working-with-json.md"
---

{{< edit_this_page >}}

Mikä ja miksi?
JSON (JavaScript Object Notation) on tiedostomuoto, jota käytetään tietojen tallentamiseen ja siirtämiseen eri ohjelmistojen välillä. Sitä käytetään usein web-sovellusten kehityksessä ja sen suosio on kasvanut huomattavasti. JSON on myös helposti luettavissa ihmisille ja sen käyttö on yleistymässä myös PowerShell-ohjelmoinnissa.

Miten:
JSON-tietojen käsittely PowerShell:ssa on helppoa ja suoraviivaista. Voit lukea ja muokata JSON-tiedostoja suoraan PowerShell-komentoriviltä tai käyttää siihen valmiita moduuleja, kuten ConvertFrom-Json ja ConvertTo-Json. Voit myös luoda JSON-dataa haluamallasi tavalla käyttämällä hashtaulukoita ja muuttujia.

```powershell
# Luodaan yksinkertainen JSON-tiedosto
$person = @{
    name = "Mikko"
    age = 25
    hobbies = @("programming", "hiking", "cooking")
}
# Muunnetaan hashtaulukko JSON-muotoon ja tallennetaan tiedostoon
$person | ConvertTo-Json | Out-File -FilePath "person.json"

# Luetaan ja tulostetaan JSON-tiedoston sisältö
$json = Get-Content -Raw -Path "person.json"
$json | ConvertFrom-Json
```

Tulostus:
```powershell
name  : Mikko
age   : 25
hobbies : {programming, hiking, cooking}
```

Syväys:
JSON kehitettiin alunperin JavaScript-kielessä, mutta siitä on tullut suosittu tiedon tallennusmuoto myös muissa ohjelmointikielissä. JSON käyttää avain-arvo pareja, jotka vastaavat JavaScript-kielen objekteja. Tämä tekee siitä luettavamman ja helpommin käsiteltävissä kuin esimerkiksi XML-tiedostot. JSON-tiedostoja käytetään usein REST API:n palauttamassa datassa ja niiden avulla voidaan siirtää tietoa eri järjestelmien välillä.

Vaihtoehtoisia tiedostomuotoja JSON:lle ovat esimerkiksi XML, CSV ja YAML. Vaikka ne voivat toimia samankaltaisina datan tallennusmuotoina, JSON on tullut suosituksi erityisesti web-sovellusten kehityksessä sen yksinkertaisuuden ja helppolukuisuuden vuoksi.

JSON-tiedostojen parsiminen ja muokkaaminen on mahdollista myös ilman moduuleja pelkällä PowerShell-komentorivillä, mutta moduulien käyttö tekee siitä huomattavasti helpompaa. Moduuleilla on myös sisäänrakennettuja turvallisuustoimintoja, jotka suojaa ohjelmoijaa mahdollisten haitallisten tiedostojen suorittamiselta.

Katso myös:
- [PowerShellin JSON-dokumentaatio](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/convertfrom-json?view=powershell-7)
- [JSON.org](https://www.json.org/json-en.html)
- [PowerShell-yhteisö](https://github.com/PowerShell/PowerShell)