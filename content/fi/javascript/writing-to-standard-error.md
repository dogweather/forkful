---
title:    "Javascript: Tietokoneohjelmoinnin artikkeli: Kirjoittaminen standardivirheelle."
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Miksi

Jos olet ohjelmoija, olet todennäköisesti törmännyt virheisiin koodissa ja olet joutunut hyödyntämään debuggausta. Tämän vuoksi kirjoittaminen standardi virheeseen, eli standard erroriin, on tärkeä taito ohjelmoinnissa. Se auttaa sinua löytämään ja korjaamaan virheitä koodissasi nopeammin ja tarkemmin.

## Kuinka kirjoittaa standardi virheeseen

Taustatietona, standardi virhe on yksi kolmesta tiedostovirheestä Javascriptissä. Se auttaa ohjelmoijia näkemään virheilmoituksia ja tracebackeja, jotka auttavat heitä ymmärtämään ja korjaamaan koodivirheitä. Alla on esimerkki koodista, jossa käytetään standardi virhettä:

```Javascript
try {
  // tähän tulee koodisi
} catch (err) {
  console.error(err); //tulostaa virheen konsoliin
}
```

Yllä oleva koodi yrittää ensin suorittaa antamasi koodin ja jos se heittää virheen, se tulostaa sen konsoliin. Tämä auttaa sinua näkemään, missä kohtaa koodissasi tapahtui virhe ja pystyt korjaamaan sen helpommin.

## Syvemmällä standardi virheessä

Koodin kirjoittaminen standardi virheeseen voi auttaa tekemään ohjelmoinnista sujuvampaa ja nopeampaa, mutta se vaatii myös jonkin verran syvempää ymmärrystä virheenkäsittelystä ja debuggaamisesta. Esimerkiksi voit myös lisätä omaa logiikkaa virheenkäsittelyyn, kuten alle:

```Javascript
try {
  // tähän tulee koodisi
} catch (err) {
  if (err.name === 'ReferenceError') {
    console.log('Anna koodille vihje, jotta virhe on helpompi korjata'); // voit lisätä oman logiikan virheenkäsittelyyn
  }
  console.error(err); // tulostaa virheen konsoliin
}
```

Voit myös lukea lisää standardi virheestä ja sen käytöstä dokumentaatiosta tai muista oppimateriaaleista.

## Katso myös

- [Virheenkäsittely JavaScriptissä (MDN)](https://developer.mozilla.org/fi/docs/Web/JavaScript/Reference/Statements/try...catch)
- [Debuggaus JavaScriptissä (MDN)](https://developer.mozilla.org/fi/docs/Web/JavaScript/Reference/Statements/debugger)
- [JavaScript-virheenkorjaus (W3schools)](https://www.w3schools.com/js/js_error.asp)