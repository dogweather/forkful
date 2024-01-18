---
title:                "Kirjoittaminen standardivirheelle"
html_title:           "Lua: Kirjoittaminen standardivirheelle"
simple_title:         "Kirjoittaminen standardivirheelle"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/lua/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Kirjoittaminen standardi virheenkorjaus (standard error) on tapa ilmoittaa virheistä ja muista tärkeistä viesteistä ohjelman suorituksen aikana. Sillä on tärkeä rooli ohjelmoijille virheiden havaitsemisessa ja korjaamisessa.

## Etsin yksin
Lua tarjoaa `io.stderr` -funktion, joka mahdollistaa kirjoittamisen standardi virheenkorjaus ikkunaan. Se ottaa vastaan tekstiä parametrinä ja tulostaa sen virheenkorjaus ikkunaan. Tässä on yksinkertainen esimerkki:

```lua
io.stderr:write("Virhe: Jokin meni pieleen!") 
```

Tämä tulostaa "Virhe: Jokin meni pieleen!" standardi virheenkorjaus ikkunaan. Huomaa, että voit käyttää myös `print()` -funktiota standardi virheenkorjaus ikkunan sijaan. Tässä on esimerkki:

```lua
print("Virhe: Jokin meni pieleen!", io.stderr)
```

Tämä tulostaa saman viestin, mutta käyttää `print()` -funktiota sen sijaan, että kirjoittaisi suoraan `io.stderr` -funktiota.

## Syvempi sukellus
Kirjoitus standardi virheenkorjaus ikkunaan voi olla hyödyllistä myös esimerkiksi debuggauksessa. Voit käyttää `assert()` -funktiota ja ohjelmoida sen avulla oman virheenilmoituksen, jos jokin ehto ei täyty. Tässä on esimerkki:

```lua
function lukuJaTarkista(luku)
  assert(type(luku) == "number", "Luku pitää olla numero!")
  print("Luku on: ", luku)
end

lukuJaTarkista("kaksi") -- Tämä aiheuttaa virheen ja tulostaa tekstin "Luku pitää olla numero!" standardi virheenkorjaus ikkunaan.
```

Standardi virheenkorjaus ikkunan sijaan voit myös käyttää `io.output()` -funktiota, jolla voit määrittää mihin tulostus tapahtuu. Voit esimerkiksi luoda oman virheenkorjaus ikkunan tiedoston, johon kirjoitat virheviestit. Voit tarkistaa Lua-oppaan lisätietoja `io`-kirjastoon liittyen.

## Katso myös
- [Lua-opas](https://www.lua.org/manual/5.4/) - Täydellinen Lua-opas, josta löydät kaiken tarvitsemasi tiedon.
- [io-kirjasto](https://www.lua.org/manual/5.4/manual.html#6.8) - Lisätietoja `io`-kirjaston käytöstä.
- [assert()-funktio](https://www.lua.org/manual/5.4/manual.html#pdf-assert) - Lisätietoja `assert()`-funktion käytöstä.