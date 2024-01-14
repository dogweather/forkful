---
title:                "Javascript: Kahden päivämäärän vertailu"
simple_title:         "Kahden päivämäärän vertailu"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Miksi

Monissa ohjelmoinnin tilanteissa, kuten varausjärjestelmissä tai aikataulutussivustoissa, on tarpeen verrata kahta eri päivämäärää. Tämä mahdollistaa esimerkiksi tiettyjen päivämäärien välisen erotuksen laskemisen tai tarkistamisen, onko jokin tietty päivämäärä jo mennyt.

## Miten

Vertaileminen kahden päivämäärän välillä on mahdollista JavaScriptillä käyttämällä Date-objekteja ja niiden metodeja. Esimerkiksi voimme luoda kaksi eri päivämäärämuuttujaa ja käyttää niiden välillä metodia "getTime()", joka palauttaa päivämäärän millisekunteina.

```Javascript
let date1 = new Date("2020-01-01");
let date2 = new Date("2020-02-01");
let difference = date2.getTime() - date1.getTime();
console.log(difference); // tulostaa 2678400000, eli 31 päivän pituisen eron millisekunteina
```

Toinen tapa verrata päivämääriä on käyttää niiden vertailuoperaattoreita, kuten ">" ja "<". Näin voimme esimerkiksi tarkistaa, onko jokin päivämäärä jo mennyt tai onko se tulevaisuudessa.

```Javascript
let now = new Date();
let futureDate = new Date("2023-01-01");
if (futureDate > now) {
  console.log("Tämä päivämäärä on tulevaisuudessa!");
} else {
  console.log("Tämä päivämäärä on jo mennyt.");
}
```

## Syvällinen tarkastelu

Päivämäärien vertailu voi joskus olla hieman hankalaa, sillä ne eivät ole vain pelkkiä lukuarvoja, vaan sisältävät myös aikavyöhykkeeseen liittyvää tietoa. Tämä voi vaikuttaa päivämäärien välisten erojen laskemiseen tai vertailuun. Lisäksi on hyvä huomata, että kaikki selaimet eivät välttämättä tue samoja päivämäärämuotoja, joten tarkista aina, että käytössäsi oleva muoto toimii kaikilla haluamillasi selaimilla.

## Katso myös

- [MDN: Date](https://developer.mozilla.org/fi/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [W3Schools: JavaScript Dates](https://www.w3schools.com/js/js_dates.asp)
- [Stack Overflow: Comparing two dates in JavaScript](https://stackoverflow.com/questions/497790/how-do-you-write-a-javascript-function-that-compare-dates)