---
title:                "Virheiden käsittely"
date:                  2024-01-26T00:52:39.955148-07:00
model:                 gpt-4-1106-preview
simple_title:         "Virheiden käsittely"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/handling-errors.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Virheenkäsittely antaa skriptillesi mahdollisuuden käsitellä odottamattomia tilanteita sulavasti. Teemme sen hallitaksemme epäonnistumisia aiheuttamatta käyttäjillemme harmaita hiuksia.

## Miten:
Virheiden kiinniottamiseksi Fishissä, nojaa `status`-komentoon ja ehtolauseisiin. Sanokaamme, että `ping` epäonnistuu; tässä on miten voit havaita sen:

```fish
ping -c 1 example.com
if not status is-success
    echo "Jokin meni mönkään pingin kanssa."
end
```

Esimerkkituloste jos `ping` epäonnistuu:

```
Jokin meni mönkään pingin kanssa.
```

Tietyn virhekoodin käsittelemiseksi, käytä `status --is`:

```fish
false
if status --is 1
    echo "Kiinni virheestä koodilla 1."
end
```

Esimerkkituloste:
```
Kiinni virheestä koodilla 1.
```

Jotta saisit aikaan kestävämmän ratkaisun, harkitse funktion käyttämistä:

```fish
function try_ping
    ping -c 1 example.com
    or begin
        echo "Ping epäonnistui tilalla $status"
        return 1
    end
end

try_ping
```

## Syventävä tarkastelu
Virheenkäsittely Fishissä ei vastaa korkeamman tason kielistä tuttua `try/catch`-paradigmaa. Sen sijaan sinulla on suoraviivaisia poistumiskoodeja `status`-komennon tarjoamana.

Historiallisesti Unix-tyyppisissä järjestelmissä nollan suuruinen poistumistila merkitsee menestystä, kun taas mikä tahansa nollasta poikkeava arvo osoittaa virhetta, joka usein heijastaa erilaisia epäonnistumisen syitä. Tätä konventiota käyttävät useimmat komentorivityökalut ja siten myös Fish itse.

Vaihtoehtoja `status`-tarkistuksille Fishissä sisältävät signaalinkäsittelyn käyttämisen `trap`-komennon avulla muissa kuorissa, mutta Fish suosii selkeämpää tilan tarkistusta, koska se on selkeämpää ja vähemmän altis sivuvaikutuksille.

Toteutuksen näkökulmasta virheiden käsittely Fishissä pysyy yksinkertaisena mutta tehokkaana, suurelta osin sen estottoman luonteen ja selkeän syntaksin korostamisen ansiosta, kuten esimerkeissä näkyy. Virhekoodit yhdistyvät hienosti funktioihin, mahdollistaen modulaarisen ja luettavan virheenhallinnan.

## Katso myös
- Fishin dokumentaatio ehdolauseista: https://fishshell.com/docs/current/language.html#conditionals
- Fishin opas virheenkäsittelyyn: https://fishshell.com/docs/current/tutorial.html#error-handling