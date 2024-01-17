---
title:                "Verkkosivun lataaminen"
html_title:           "Javascript: Verkkosivun lataaminen"
simple_title:         "Verkkosivun lataaminen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Mitä ja miksi?

Tiedätkö kuinka voit ladata web-sivun näytöllesi? Tämä on yksi tärkeimmistä taidoista, joita ohjelmoijan tulisi hallita. Tämä tarkoittaa sitä, että voit hakea ja näyttää web-sivuja internetistä. Tämä on oleellista monille sovelluksille, kuten selaimille, hakukoneille ja sosiaalisen median sovelluksille.

# Kuinka tehdä?

Pystyt lataamaan web-sivuja käyttämällä `JavaScript`-kielen sisäänrakennettua toimintoa `XMLHttpRequest`. Voit aloittaa luomalla uuden XMLHttpRequest-olion ja asettamalla sen avulla osoitteen, josta haluat ladata web-sivun. Sitten käytä `open()`-metodia määrittämään HTTP-pyyntötyyppi ja osoitteen. Lopuksi käytä `send()`-metodia aloittaaksesi lataamisen. Alla on esimerkki koodista:

```Javascript
let xmlhttp = new XMLHttpRequest(); // Luodaan uusi olio
xmlhttp.open('GET', 'https://example.com', true); // Määritetään HTTP-pyyntötyyppi ja osoite
xmlhttp.send(); // Aloittaa lataamisen
```

Kun olet ladannut web-sivun, voit käyttää `responseText`-ominaisuutta saadaksesi sivun sisällön. Voit myös käyttää muita ominaisuuksia, kuten `status`, joka kertoo lataamisen tilasta, tai `getAllResponseHeaders()`, joka palauttaa kaikki vastauksen otsikot. Alla on esimerkki koodista, joka tulostaa ladatun sivun sisällön:

```Javascript
// Kun ladattu sivu on valmis
xmlhttp.onreadystatechange = function() {
    if (xmlhttp.readyState == 4 && xmlhttp.status == 200) {
        // Tulostetaan sivun sisältö
        console.log(xmlhttp.responseText);
    }
}
```

# Syvällinen sukellus

`XMLHttpRequest`-tekniikka on ollut käytössä vuodesta 1999 lähtien ja se on yksi ensimmäisistä tekniikoista, joita käytettiin lataamaan web-sivuja dynaamisesti. Nykyään on olemassa myös muita vaihtoehtoja, kuten käyttämällä `Fetch API`-toimintoa tai asynkronista `fetch()`-metodia. Nämä toimivat samalla periaatteella kuin `XMLHttpRequest`.

Kun lataat web-sivun, voit myös määrittää muita asetuksia, kuten asettaa HTTP-otsikoita lisämällä `setRequestHeader()`-metodin. Voit myös määrittää pyynnön asetukset, kuten käyttämällä `withCredentials`-ominaisuutta määrittääksesi, kuljetetaanko evästeitä pyynnössä. Voit löytää lisätietoja näistä vaihtoehdoista MDN-dokumentaatiosta.

# Katso myös

- [MDN - Asynchronous JavaScript](https://developer.mozilla.org/en-US/docs/Learn/JavaScript/Asynchronous)
- [W3Schools - XMLHttpRequest](https://www.w3schools.com/XML/xml_http.asp)
- [Fetch API](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API)