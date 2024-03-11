---
date: 2024-01-26 03:48:23.573625-07:00
description: "Debuggerin k\xE4ytt\xE4minen tarkoittaa, ett\xE4 varustaudut suurennuslasilla\
  \ tarkastelemaan koodiasi. Ohjelmoijat tekev\xE4t t\xE4m\xE4n paikatakseen bugeja,\
  \ ymm\xE4rt\xE4\xE4kseen\u2026"
lastmod: '2024-03-11T00:14:30.118272-06:00'
model: gpt-4-0125-preview
summary: "Debuggerin k\xE4ytt\xE4minen tarkoittaa, ett\xE4 varustaudut suurennuslasilla\
  \ tarkastelemaan koodiasi. Ohjelmoijat tekev\xE4t t\xE4m\xE4n paikatakseen bugeja,\
  \ ymm\xE4rt\xE4\xE4kseen\u2026"
title: "Debuggerin k\xE4ytt\xF6"
---

{{< edit_this_page >}}

## Mikä ja miksi?
Debuggerin käyttäminen tarkoittaa, että varustaudut suurennuslasilla tarkastelemaan koodiasi. Ohjelmoijat tekevät tämän paikatakseen bugeja, ymmärtääkseen virtausta ja varmistaakseen, että heidän logiikkansa toimii odotetulla tavalla.

## Kuinka:
Clojure nojaa Java Virtuaalikoneeseen (JVM), joten suuri osa debuggauksesta tapahtuu Java-työkaluilla. Yksi tällainen työkalu on `CIDER`, tehokas paketti Clojure-kehitykseen Emacsissa, jolla on vankat debuggauskyvyt. Sukellamme sisään:

```clojure
;; Ensimmäiseksi, kytkeydy Clojure-projektiin Emacsin sisällä käyttäen CIDERia
M-x cider-jack-in

;; Aseta katkaisukohta
;; Navigoi riviin Clojure-koodissasi, jota haluat tarkastella ja
;; paina "C-c M-b" tai suorita:
M-x cider-debug-defun-at-point

;; Kun koodi suorittuu, osut katkaisukohtaan. CIDER kysyy sinulta:
;; 1. n mennäksesi seuraavaan loogiseen askelmaan suorituksessa,
;; 2. c jatkaaksesi suoritusta seuraavaan katkaisukohtaan,
;; 3. q lopettaaksesi debuggauksen.

;; Tarkasta paikalliset muuttujat katkaisukohdassa
;; Ollessasi katkaisukohdassa, kirjoita:
locals

;; Näet listan paikallisia muuttujia ja niiden arvoja tulostettuna minibufferiin.
```
Esimerkkituloste voi näyttää:
```clojure
{:x 10, :y 20, :result 200}
```

## Syväsukellus
Debugger on työkalu, joka on ollut olemassa ikuisuuden tietokonealan termeissä. Termi "bugi" keksittiin tietokoneiden alkuaikoina, kun todellinen hyönteinen aiheutti virheen oikosulkemalla laitteen piirin.

Vaikka `CIDER` on loistava työkalu Emacs-harrastajille, Clojuren debuggaukseen on muitakin vaihtoehtoja. Esimerkiksi IntelliJ:n käyttäminen Cursive-laajennoksen kanssa voi tarjota enemmän GUI-pohjaista debuggauskokemusta. Lisäksi voit käyttää sisäänrakennettua Leiningeniä tai tools.deps:iä hallitaksesi prosessivirtaa debuggauksen aikana.

Näiden debuggerien alla ne usein manipuloivat baittikoodeja, suorittavat arviointeja omistetuissa nREPL-istunnoissa ja tarjoavat pinonjäljityksen tarkasteluja. Ne hyödyntävät alla olevan JVM:n kyvykkyyksiä, hyödyntämällä Java:n debuggauskehyksen rikkautta.

## Katso myös
- [CIDER Debugger -dokumentaatio](https://docs.cider.mx/cider/debugging/debugger.html)
- [Cursive Debugger](https://cursive-ide.com/userguide/debugging.html)
- [Leiningen automaatioon ja debuggaukseen](https://leiningen.org/)
- [tools.deps.alpha parempaa hallintaa varten](https://github.com/clojure/tools.deps.alpha)
