---
title:                "Tekstin etsiminen ja korvaaminen"
html_title:           "Arduino: Tekstin etsiminen ja korvaaminen"
simple_title:         "Tekstin etsiminen ja korvaaminen"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Tekstin hakeminen ja korvaaminen on prosessi, jossa määritetty tekstimerkkijono löydetään ja korvataan uudella. Ohjelmoijat tekevät tämän välttääkseen virheitä ja tehostaakseen työnsä koodin sisällä.

## Miten:

Käytettäessä PowerShellia tekstin korvaamiseen, voimme käyttää seuraavaa komentoa:

```PowerShell
$muuttuja = "Tervetuloa PowerShell-kurssiin!"
$muuttuja = $muuttuja -replace 'PowerShell', 'Ohjelmointi'
```

Tulostuksena saamme seuraavan:

```PowerShell
"Tervetuloa Ohjelmointi-kurssiin!"
```
Toinen esimerkki:

```PowerShell
$c = "Ohjelmointi on hauskaa!"
$c = $c -replace "hauskaa", "mielenkiintoista"
```

Tuloksena:

```PowerShell
"Ohjelmointi on mielenkiintoista!"
```
## Syvä sukellus:

- Historiallinen tausta: Tekstin etsimis- ja korvaamistoiminto on ollut olemassa ohjelmoinnin alkuajoista saakka. Se on yksinkertainen mutta välttämätön työkalu, jota on käytetty kaikilla ohjelmointikielillä.

- Vaihtoehdot: PowerShell ei ole ainoa vaihtoehto tekstinkorvaukseen. Muita vaihtoehtoja ovat esimerkiksi Pythonin `re.sub()`, JavaScriptin `replace()` ja C# `Replace()` -metodit.

- Toteutustiedot: PowerShellin `-replace` -operaattori käyttää säännöllistä lausetta tekstin korvaamiseen. Tämä tarkoittaa, että voit käyttää monimutkaisempia hakukuvioita paljon joustavammin kuin pelkillä merkkijonoilla.

## Katso myös:

- [PowerShell Replace Method Documentation](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/replace?view=powershell-7.1)
- [Regex Replace in Python](https://docs.python.org/3/library/re.html#re.sub)
- [JavaScript Replace Function](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [C# Replace Method](https://docs.microsoft.com/en-us/dotnet/api/system.string.replace?view=net-5.0)