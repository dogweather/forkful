---
title:                "Assosiatiivisten taulukoiden käyttö"
aliases:
- /fi/javascript/using-associative-arrays/
date:                  2024-01-30T19:12:04.755768-07:00
model:                 gpt-4-0125-preview
simple_title:         "Assosiatiivisten taulukoiden käyttö"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä ja miksi?

Assosiatiiviset taulukot eli niin kuin Javascriptissä tarkemmin tunnetaan, objektit, mahdollistavat avainten määrityksen arvoille. Tämä on erittäin kätevää, kun tarvitset elementtien kokoelman, johon haluat päästä käsiksi tiettyjen nimien (avaimien) kautta numeeristen indeksien sijaan, mikä tekee koodistasi luettavampaa ja joustavampaa.

## Kuinka:

Assosiatiivisten taulukoiden (objektien) luominen ja käyttäminen Javascriptissä on suoraviivaista. Määrität objektin käyttäen aaltosulkeita `{}`, ja niiden sisällä voit määrittää joukon avain-arvo -pareja. Avaimet ovat aina merkkijonoja, ja arvot voivat olla mitä tahansa: merkkijonoja, numeroita, taulukoita, jopa muita objekteja.

```javascript
// Assosiatiivisen taulukon luominen
let userInfo = {
  name: "Alex",
  age: 30,
  email: "alex@example.com"
};

// Elementtien käyttäminen
console.log(userInfo.name); // Tuloste: Alex
console.log(userInfo["email"]); // Tuloste: alex@example.com

// Uusien elementtien lisääminen
userInfo.job = "Kehittäjä";
userInfo["maa"] = "Kanada";

console.log(userInfo);
/* Tuloste:
{
  name: "Alex",
  age: 30,
  email: "alex@example.com",
  job: "Kehittäjä",
  maa: "Kanada"
}
*/

// Elementin poistaminen
delete userInfo.age;
console.log(userInfo);
/* Tuloste:
{
  name: "Alex",
  email: "alex@example.com",
  job: "Kehittäjä",
  maa: "Kanada"
}
*/
```

Kuten näet, elementtien käyttäminen, lisääminen tai poistaminen assosiatiivisessa taulukossa on melko suoraviivaista ja intuitiivista.

## Syventävä tarkastelu

JavaScript-maailmassa, vaikka termiä "assosiatiivinen taulukko" usein kuuleekin, teknisesti se on väärinkäsitys, sillä JavaScript ei omaa todellisia assosiatiivisia taulukoita kuten muut kielet (esim. PHP). JavaScriptissä on objekteja, jotka palvelevat samankaltaista tarkoitusta, mutta ovat voimakkaampia ja joustavampia rakenteita.

Historiallisesti ohjelmointikielissä taulukot on suunniteltu pitämään kokoelma kohteita, joihin päästään käsiksi niiden numeerisen indeksin kautta. Kuitenkin, kun ohjelmistokehitys on kehittynyt, tarve joustavammille tietorakenteille on ilmennyt. Assositatiiviset taulukot tai sanakirjat muissa kielissä, olivat yksi vastaus, mahdollistaen elementtien käytön mielivaltaisten avainten kautta.

JavaScriptin lähestymistapa käyttäen objekteja avain-arvo -varastoina tarjoaa toiminnallisuuden sekoituksen. Se mahdollistaa ominaisuuksien (avaimien) lisäämisen, poistamisen ja etsimisen nimellä. JSON (JavaScript Object Notation) on todiste tämän rakenteen hyödyllisyydestä, tullen de facto standardiksi datan vaihtoon verkossa.

Vaikka objektit kattavat suurimman osan assosiatiivisten taulukoiden tarpeista, tapauksissa, joissa avaimen järjestys tai iterointi on tärkeää, ES6:ssa esitelty `Map`-objekti tarjoaa paremman vaihtoehdon. `Map` säilyttää avaimen järjestyksen, hyväksyy laajemman valikoiman datatyyppien avaimia ja sisältää hyödyllisiä metodeja iteroinnille ja koon noutamiselle. Huolimatta näistä eduista, perinteinen objektisyntaksi pysyy suosittuna sen yksinkertaisuuden ja helpon käytettävyyden vuoksi monissa yleisissä skenaarioissa.
