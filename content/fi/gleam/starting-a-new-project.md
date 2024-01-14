---
title:    "Gleam: Uuden projektin aloittaminen"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Miksi

Aloittaessa uuden projektin, usein kysytään itseltään miksi? Miksi aloittaa tyhjästä ja luoda jotain uutta? Syitä voi olla monia, mutta yksi tärkeimmistä on halu oppia uutta ja haastaa itsensä. Gleam tarjoaa mahdollisuuden luoda tehokasta ja turvallista koodia, joka voi olla erittäin motivoivaa ja antoisaa.

## Miten

Gleam on selkeästi ja yksinkertaisesti kirjoitettu ohjelmointikieli, joka käyttää funktionaalista ohjelmointiparadigmaa. Tämä tarkoittaa, että koodi koostuu pienistä toiminnallisista palikoista, jotka tekevät yhden asian hyvin. Alla on esimerkki siitä, miten voit luoda uuden moduulin Gleamissa:

```Gleam
// Tämä on uusi moduuli, joka tulostaa tervehdyksen
moduuli Tervehdys {
    uloskirjautuminen tervehdys() {
        konsoli.loggaus("Hei Gleam lukijat!");
    }
}

```

Kun olet luonut tämän moduulin, voit suorittaa sen komentoriviltä ja näet seuraavan tuloksen:

```Gleam
`henkilökohtaisen tervehdyksen()
Hei Gleam lukijat!
```

Gleamin yleinen syntaksi on hyvin samanlainen kuin muissa ohjelmointikielissä, joten sen oppiminen ei vaadi paljon aikaa. Voit tutustua Gleamin viralliseen oppaaseen [täältä](https://gleam.run/book/introduction.html) ja aloittaa uuden projektin luomisen.

## Syväsukellus

Syvemmälle sukeltaessasi uuden projektin luomiseen Gleamissa, huomaat, että sen paketinhallinta ja riippuvuuksien hallinta ovat erittäin tehokkaita ja helposti ymmärrettäviä. Voit luoda uuden projektin luomalla uuden kansion ja suorittamalla komennon `gleam init`. Tämä luo Gleam-projektillesi rakenteen ja automaattisesti luo `gunglefile.toml`-tiedoston, johon voit lisätä haluamasi riippuvuudet.

Gleamin yhteisö on myös hyvin aktiivinen ja siellä on paljon resursseja, kuten [Github-repositorioita](https://github.com/gleam-lang) ja [Slack-kanavia](https://gleam-lang.slack.com/), joissa voit saada apua uuden projektisi kanssa.

## Katso myös

- [Gleamin virallinen opas](https://gleam.run/book/introduction.html)
- [Github-repositorioita Gleam-projekteista](https://github.com/gleam-lang)
- [Slack-kanava Gleam-yhteisölle](https://gleam-lang.slack.com/)