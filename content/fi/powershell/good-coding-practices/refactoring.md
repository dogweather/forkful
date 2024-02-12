---
title:                "Koodin refaktorointi"
aliases:
- /fi/powershell/refactoring.md
date:                  2024-01-26T03:37:20.375991-07:00
model:                 gpt-4-0125-preview
simple_title:         "Koodin refaktorointi"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/refactoring.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Refaktorointi on prosessi, jossa olemassa olevaa tietokonekoodia uudelleenjärjestetään muuttamatta sen ulkoista käyttäytymistä, pyrkimyksenä parantaa ohjelmiston ei-toiminnallisia ominaisuuksia. Ohjelmoijat refaktoroivat koodia tehdäkseen siitä selkeämpää, tehokkaampaa ja helpommin ymmärrettävää, mikä helpottaa ylläpitoa ja tulevia parannuksia.

## Miten:
PowerShell ei sisällä erityistä refaktorointityökalua, mutta voit silti siistiä koodiasi luettavuuden ja suorituskyvyn parantamiseksi. Harkitse funktion, joka tekee liikaa ja miten voisimme refaktoroida sitä selkeyden vuoksi:

```PowerShell
function Get-InventoryData {
    # Alkuperäinen funktio, joka yhdistää datan noudon ja muotoilun
    $data = Get-Content -Path 'C:\inventory-list.txt'
    $inventoryData = $data | ForEach-Object {
        $fields = $_ -split ','
        [PSCustomObject]@{
            ItemID = $fields[0]
            Name   = $fields[1]
            Count  = $fields[2]
            Price  = $fields[3]
        }
    }
    $inventoryData | Format-Table -AutoSize
}

# Refaktoroitu erillisiin funktioihin
function Import-InventoryData {
    param($Path)
    Get-Content -Path $Path | ForEach-Object {
        $fields = $_ -split ','
        [PSCustomObject]@{
            ItemID = $fields[0]
            Name   = $fields[1]
            Count  = $fields[2]
            Price  = $fields[3]
        }
    }
}

function Format-InventoryData {
    param($Data)
    $Data | Format-Table -AutoSize
}

# Käyttö
$inventory = Import-InventoryData -Path 'C:\inventory-list.txt'
Format-InventoryData -Data $inventory
```

Esimerkkituloste:

```
ItemID Name            Count Price
------ ----            ----- -----
1001   Widget Type A   50    9.99
1002   Gadget Type B   20    14.99
```

## Syvä sukellus
Refaktoroinnilla ohjelmoinnissa on juurensa ohjelmistokehityksen alkuaikoina, vaikka se muodollistettiin käytännöksi 1990-luvulla. Martin Fowlerin kirja "Refactoring: Improving the Design of Existing Code" on yksi aiheen perusteos, joka korostaa refaktoroinnin tärkeyttä puhtaan koodin saavuttamisessa.

Vaikka PowerShell ei tulekaan erityisten refaktorointityökalujen kanssa, kuten jotkin muiden kielten integroidut kehitysympäristöt (IDEt) tekevät (ajattele Eclipse tai Visual Studio), voit silti harjoittaa hyviä refaktorointiperiaatteita manuaalisesti. Tärkeintä on muistaa, että refaktorointi ei ole vain koodin muuttamista muuttamisen vuoksi, vaan tarkoituksellisten, käyttäytymistä säilyttävien muutosten tekemistä, jotka parantavat koodin rakennetta ja suunnittelua.

Vaihtoehtoja manuaaliselle refaktoroinnille PowerShellissä sisältää IDEiden käyttö, jotka tukevat kieltä, kuten Visual Studio Code PowerShell-laajennuksella, joka tarjoaa ominaisuuksia, kuten koodin muotoilu ja perusrefaktorointikyvykkyydet. Merkittävämmän refaktoroinnin osalta saattaisit harkita Pester-testien käyttöä varmistamaan, etteivät muutokset muuta toiminnallisuutta.

Lisäksi, refaktoroinnin toteutus voi sisältää enemmän järjestelmällisiä muutoksia kuten modularisoinnin, jossa koodi jaetaan uudelleenkäytettäviin moduuleihin tai funktioihin, parantaen DRY (Älä Toista Itseäsi) -periaatteen noudattamista. Muita yleisiä refaktorointitekniikoita ovat nimeämisen selventäminen, duplikaattikoodin poistaminen ja ehdollisen logiikan monimutkaisuuden vähentäminen.

## Katso myös
Sukellus syvemmälle, tässä joitain resursseja:

- Martin Fowlerin Refaktorointi-kirja: [_Refactoring: Improving the Design of Existing Code_](https://martinfowler.com/books/refactoring.html)
- Refaktoroidun koodin testaaminen Pesterillä: [Pester-testauskehys](https://pester.dev/)
- PowerShellin parhaat käytännöt: [PowerShellin parhaat käytännöt ja tyyliopas](https://poshcode.gitbooks.io/powershell-practice-and-style/)
