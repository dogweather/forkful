---
date: 2024-01-26 01:18:42.817301-07:00
description: "Refaktorointi on k\xE4yt\xE4nn\xF6ss\xE4 koodikannan kev\xE4tsiivous\
  \ \u2013 siin\xE4 uudelleenj\xE4rjestet\xE4\xE4n olemassa olevaa koodia muuttamatta\
  \ sen ulkoista toimintaa. Ohjelmoijat\u2026"
lastmod: '2024-03-13T22:44:56.497022-06:00'
model: gpt-4-0125-preview
summary: "Refaktorointi on k\xE4yt\xE4nn\xF6ss\xE4 koodikannan kev\xE4tsiivous \u2013\
  \ siin\xE4 uudelleenj\xE4rjestet\xE4\xE4n olemassa olevaa koodia muuttamatta sen\
  \ ulkoista toimintaa. Ohjelmoijat\u2026"
title: Koodin refaktorointi
weight: 19
---

## Mikä & Miksi?
Refaktorointi on käytännössä koodikannan kevätsiivous – siinä uudelleenjärjestetään olemassa olevaa koodia muuttamatta sen ulkoista toimintaa. Ohjelmoijat tekevät sitä tehdäkseen koodista luettavampaa, vähentääkseen monimutkaisuutta, parantaakseen ylläpidettävyyttä ja helpottaakseen sen laajentamista.

## Kuinka:
Kuvittele, että sinulla on Elm-funktio, joka tekee liikaa, kuten sekoittaa käyttöliittymälogiikkaa tilan päivityksiin. Se on täydellinen kandidaatti refaktorointiin. Alun perin:

```Elm
updateAndFormat : String -> Model -> (Model, Cmd Msg)
updateAndFormat input model =
    let
        updatedModel = { model | userInput = input }
    in
    if String.length input > 5 then
        ( updatedModel, Cmd.none )
    else
        ( model, Cmd.none )
```

Refaktoroinnin jälkeen erotamme huolenaiheet vetämällä logiikan eri funktioihin:

```Elm
-- Päivityslogiikka on erillään
updateUserInput : String -> Model -> Model
updateUserInput input model = 
    { model | userInput = input }

-- Käyttäjäsyötteen muotoilu (näkymä) logiikka on myös erillään
formatUserInput : Model -> (Model, Cmd Msg)
formatUserInput model =
    if String.length model.userInput > 5 then
        ( model, Cmd.none )
    else
        ( { model | userInput = "" }, Cmd.none ) -- Tyhjennä syöte, jos se on liian lyhyt, esimerkkisääntönä.

-- Päivitysfunktio käyttää nyt apufunktioita
updateAndFormat : String -> Model -> (Model, Cmd Msg)
updateAndFormat input model =
    model
    |> updateUserInput input
    |> formatUserInput
```
Näillä muutoksilla sinulla on selvä erottelu, ja jokainen funktio on helpompi ymmärtää ja testata.

## Syväsukellus
Refaktorointia muodollisena käytäntönä voidaan jäljittää ohjelmoinnin alkuaikoihin, jolloin koodin muuttamisen kustannuksia alettiin jo tunnistaa kehitysprosessin kriittisenä osana. Erityisesti Martin Fowlerin kirja "Refactoring: Improving the Design of Existing Code", julkaistu 1990-luvun lopulla, todella asetti näyttämön refaktoroinnille rakenteellisella lähestymistavalla ja "koodihajujen" luettelolla tunnistaakseen refaktorointimahdollisuuksia.

Elmin kontekstissa refaktorointi hyödyntää kielen vahvuuksia, kuten sen vahvaa tyypitysjärjestelmää, joka edistää luottamusta prosessin aikana. Manuaalisen refaktoroinnin vaihtoehtoihin voi kuulua automaattisia koodimuunnostyökaluja, mutta Elmin työkalut tällä alueella ovat vielä kypsymässä verrattuna joitakin vanhempia kieliä. Toteutusyksityiskohdat keskittyvät usein yleisiin refaktorointeihin, kuten funktion erotteluun, uudelleennimeämiseen ja ehtolausekkeiden yksinkertaistamiseen. Elmin kääntäjä on tärkeä liittolainen refaktoroinnissa, sillä se ei anna tehdä paljoakaan virheitä – se huutaa aina, kun jokin on pielessä, varmistaen, että refaktoitu koodisi toimii edelleen.

## Katso Myös
- ["Refactoring: Improving the Design of Existing Code" Martin Fowlerilta](https://martinfowler.com/books/refactoring.html)
- [Elm Discourse - Keskusteluaiheita Refaktoroinnista](https://discourse.elm-lang.org/search?q=refactoring)
