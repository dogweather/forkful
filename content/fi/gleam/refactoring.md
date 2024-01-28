---
title:                "Koodin refaktorointi"
date:                  2024-01-26T01:18:26.670146-07:00
model:                 gpt-4-0125-preview
simple_title:         "Koodin refaktorointi"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/refactoring.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Uudelleenjärjestely tarkoittaa koodisi uudelleentöitämistä, jotta se olisi selkeämpää, ylläpidettävämpää, muuttamatta sen ulkoista toimintaa. Ohjelmoijat uudelleenjärjestelevät parantaakseen luettavuutta, vähentääkseen monimutkaisuutta ja tehdäkseen koodikannasta helpommin päivitettävän tai ominaisuuksien lisäyksiin soveltuvamman.

## Kuinka:
Oletetaan, että sinulla on koodinpätkä, jossa suoritat joitakin toistuvia laskelmia tai merkkijonojen manipulointeja useiden funktioiden läpi. Se on ensiluokkainen kohde uudelleenjärjestelylle. Tässä on ennen ja jälkeen esimerkki käyttäen Gleamia, joka korostaa vahvasti tyypin turvallisuutta ja muuttumattomuutta:

```gleam
// Ennen uudelleenjärjestelyä
pub fn calculate_area(width: Int, height: Int) -> Int {
  width * height
}

pub fn print_area(width: Int, height: Int) {
  let area = calculate_area(width, height)
  io.println("Ala on \(area)")
}

// Uudelleenjärjestelyn jälkeen
pub fn calculate_area(width: Int, height: Int) -> Int {
  width * height
}

pub fn print_area(area: Int) {
  io.println("Ala on \(area)")
}

// Toisessa koodisi osassa, kutsut print_area näin:
print_area(calculate_area(10, 20))
```

Esimerkkituloste:
```
Ala on 200
```

Uudelleenjärjestelyn myötä olemme tehneet `print_area` funktiosta keskittyneemmän vain tulostamiseen, kun taas laskenta hoidetaan muualla, mikä tekee koodista modulaarisempaa ja helpommin uudelleen käytettävää tai testattavaa.

## Syväsukellus
Uudelleenjärjestely-konsepti on ollut olemassa yhtä kauan kuin ohjelmointi itse—koodin uudelleentarkastelu ja siivoaminen kuuluvat hyvän kodinhoidon piiriin. Nykyaikaisen uudelleenjärjestelyn formalisointi, sekä monet tänään käytetyt tekniikat ja mallit, voidaan jäljittää Martin Fowlerin merkittävään kirjaan "Uudelleenjärjestely: Olemassa olevan koodin suunnittelun parantaminen", joka julkaistiin vuonna 1999.

Gleam-ekosysteemissä uudelleenjärjestelyllä on tiettyjä erityisnäkökohtia. Yksi merkittävimmistä on vahva tyypin tarkistus käännösaikana, mikä voi auttaa havaitsemaan virheet aikaisin, kun siirrät asioita ympäri. Gleamin mallin vastaavuuden tarkistus ja muuttumattomuusominaisuudet voivat myös ohjata sinua kirjoittamaan selkeämpää, tiiviimpää koodia—yksi uudelleenjärjestelyn päätavoitteista.

Vaihtoehtoja uudelleenjärjestelylle voivat sisältää koodin kirjoittamisen alusta tai koodin paikkaamisen nopeilla korjauksilla. Uudelleenjärjestely on kuitenkin yleensä turvallisin ja tehokkain tapa parantaa olemassa olevaa koodia ilman uusien bugien esittelyä, koska se sisältää inkrementaalisia, hyvin perusteltuja, käyttäytymistä säilyttäviä muutoksia.

## Katso myös
- Martin Fowlerin kirja "Uudelleenjärjestely": https://martinfowler.com/books/refactoring.html
- Gleam-kielen verkkosivusto, joka sisältää lisädokumentaatiota ja esimerkkejä: https://gleam.run/
- "Uudelleenjärjestely: Olemassa olevan koodin suunnittelun parantaminen" Martin Fowlerilta (soveltuu yleisiin periaatteisiin kaikilla kielillä): https://martinfowler.com/books/refactoring.html
