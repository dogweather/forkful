---
date: 2024-01-26 03:50:07.117440-07:00
description: "Kuinka: Fish ei sis\xE4ll\xE4 sis\xE4\xE4nrakennettua debuggeria kuten\
  \ jotkin muut komentorivitulkki, mutta voit k\xE4ytt\xE4\xE4 ulkoisia ty\xF6kaluja\
  \ kuten `gdb` kompiloitujen\u2026"
lastmod: '2024-03-13T22:44:56.999277-06:00'
model: gpt-4-0125-preview
summary: "Fish ei sis\xE4ll\xE4 sis\xE4\xE4nrakennettua debuggeria kuten jotkin muut\
  \ komentorivitulkki, mutta voit k\xE4ytt\xE4\xE4 ulkoisia ty\xF6kaluja kuten `gdb`\
  \ kompiloitujen ohjelmien debuggaamiseen tai `fish -d` ajamaan fishi\xE4 debug-tulostuksella\
  \ eri tasoilla."
title: "Debuggerin k\xE4ytt\xF6"
weight: 35
---

## Kuinka:
Fish ei sisällä sisäänrakennettua debuggeria kuten jotkin muut komentorivitulkki, mutta voit käyttää ulkoisia työkaluja kuten `gdb` kompiloitujen ohjelmien debuggaamiseen tai `fish -d` ajamaan fishiä debug-tulostuksella eri tasoilla. Käytetään `fish -d`:

```fish
# Aja fish-komentorivitulkki debug-tasolla 2
fish -d2

# Fish-komentorivitulkissa, testataan yksinkertainen funktio mahdollisen vian kanssa
function test_func
    set val 42
    echo "Arvo on $val"
    if test $val -eq 42
        echo "Kaikki on hyvin."
    else
        echo "Jokin on fishynä."
    end
end

# Kutsu funktiota ja tarkkaile debug-tulostusta
test_func
```

Näkisit ylimääräistä debug-tulostusta ennen ja jälkeen funktion suorituksen, auttaen sinua paikantamaan ongelmia.

## Syvä sukellus
Historiallisesti vianetsintä Unix-tyyppisissä ympäristöissä on ollut erikoistyökalujen, kuten `gdb` C/C++:lle tai `pdb` Pythonille, alaa. Fishissä tukeudut yleensä ulkoisiin työkaluihin tai sisäänrakennettuihin ominaisuuksiin kuten `functions -v` funktioiden verboosisen tulostuksen ja `set -x` muuttujan muutosten seurannan suhteen.

Jotkut valitsevat vaihtoehtoisia komentorivitulkkia, kuten Bash, ominaisuuksien, kuten `set -x` skriptien debuggaamisen, vuoksi. Kuitenkin, Fishillä on oma viehätyksensä keskittyessään käyttäjäystävällisyyteen ja vuorovaikutteisuuteen, mikä voi vähentää kovan luokan debuggauksen tarvetta monissa tilanteissa.

Toteutuksen kannalta, skriptin debuggaus usein sisältää sen ajamisen verboosen tulostuksen kanssa ja seuraamisen, missä muuttujia asetetaan, poistetaan tai muunnetaan odottamattomilla tavoilla. Fishin värikoodatun tulostuksen ja käyttäjäystävällisen lähestymistavan ansiosta voit usein välttää debuggauksen nitty-gritty - mutta kun olet jumissa, muista, että verboisuus ja selkeys ovat parhaat työkalusi.

## Katso myös
Tässä ovat joitakin luotettavia pelastusrenkaita, kun olet syvällä koodissa:

- Fishin dokumentaatio debuggauksesta: https://fishshell.com/docs/current/index.html#debugging
- GDB (GNU Debugger) virallinen opas: https://www.gnu.org/software/gdb/documentation/
- Stack Overflow Fish-tagi - oikean maailman debuggaustapaukset: https://stackoverflow.com/questions/tagged/fish
- Edistynyt Bash-Scripting Guide - debuggauslähestymistapojen vertailuun: https://tldp.org/LDP/abs/html/debugging.html
