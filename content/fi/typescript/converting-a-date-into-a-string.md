---
title:                "Päivämäärän muuttaminen merkkijonoksi"
html_title:           "TypeScript: Päivämäärän muuttaminen merkkijonoksi"
simple_title:         "Päivämäärän muuttaminen merkkijonoksi"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Päivämäärän muuntaminen merkkijonoksi tarkoittaa päivämäärän muuttamista lukuarvoksi, joka voidaan helposti esittää merkkijonona. Tätä tehdään, jotta päivämäärää voidaan käsitellä ja näyttää helposti eri muodoissa. Ohjelmoijat tekevät tämän helpottaakseen sovelluksiensa käyttäjän päivämäärätietojen hallintaa ja näyttämistä.

## Miten:
Esimerkiksi TypeScriptin avulla voi muuntaa päivämäärän merkkijonoksi käyttämällä Date-luokan toLocaleString()-metodia. Tämä metodi hyväksyy parametreikseen kielikoodin ja vaihtoehdot, kuten päivämäärän muodon tai aikavyöhykkeen. Alla on esimerkki koodista ja sen tulostamasta merkkijonosta:

```TypeScript
const date = new Date();
console.log(date.toLocaleString("fi-FI", {  
    day: "numeric", 
    month: "long", 
    year: "numeric"
}));
//tulostaa esimerkiksi: "1. maaliskuuta 2022"
```

## Syväsukellus:
Päivämäärän muuntaminen merkkijonoksi on tapahtunut jo vuosikymmenien ajan. Ennen oli yleistä käyttää erillisiä kirjastoja tai lisäohjelmia muuntamiseen, mutta nykyään suurin osa ohjelmointikielistä sisältää valmiita toimintoja päivämäärän käsittelyyn. Päivämäärän muuntamisessa on myös useita erilaisia tapoja ja formaatteja, joista ohjelmoijat voivat valita sopivimman. Toisin kuin monissa muissa kielissä, TypeScriptin Date-luokan toLocaleString()-metodi hyväksyy myös vaihtoehtoja aikavyöhykkeelle.

## Katso myös:
Microsoftin TypeScript-dokumentaatio: https://www.typescriptlang.org/docs/handbook/standard-library.html#date