---
title:                "Javascript: Verkkosivun lataaminen"
simple_title:         "Verkkosivun lataaminen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Miksi?

Web-sivujen lataaminen on välttämätöntä monissa eri tapauksissa, kuten tiedon hankkimisessa, tietojen analysoinnissa tai sisällön hakemisessa. Se on myös tärkeä osa web-sivujen kehittämistä ja testaamista.

## Miten?

Lataa web-sivu käyttämällä Javascript-komentoa `fetch` ja sen palauttamaa `Response`-objektia. Käytä sitten `text()`-metodia hakeaksesi sivun sisällön ja tallentamalla sen muuttujaan. Katso esimerkki alla:

```Javascript
fetch("https://example.com").then(response => response.text()).then(data => {
  console.log(data); // tulostaa web-sivun sisällön konsoliin
});
```

Voit myös käyttää `async` ja `await`-avainsanoja tehdäksesi prosessista helpompaa ja odottaa vastauksen saapumista ennen sisällön hakemista. Katso esimerkki alla:

```Javascript
async function lataaSivu() {
  let vastaus = await fetch("https://example.com");
  let sisalto = await vastaus.text();
  console.log(sisalto);
}

lataaSivu(); // kutsuu funktiota ja tulostaa web-sivun sisällön konsoliin
```

## Syväsukellus

Web-sivun lataaminen käyttämällä Javascript-komentoa `fetch` tarjoaa paljon erilaisia mahdollisuuksia ja vaihtoehtoja. Voit esimerkiksi käyttää erilaisia `Response`-objektin metodeja, kuten `json()`, `blob()` tai `arrayBuffer()` saadaksesi sivun sisällön eri muodoissa. Voit myös asettaa erilaisia parametreja, kuten otsikoita ja metodityyppejä, `fetch`-komennon sisällä.

Lisäksi voit käyttää muita Javascript-komentoja, kuten `XMLHttpRequest` tai `jQuery.ajax`, saavuttaaksesi saman tavoitteen. On tärkeää huomata, että web-sivujen lataamiseen voi liittyä myös tietoturvariskejä, ja se tulisi suorittaa vain luotetuilta lähteiltä.

## Katso myös

- [MDN Web-sivujen lataaminen käyttämällä Javascriptiä](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API)
- [W3Schools JavaScript `fetch`-opas](https://www.w3schools.com/js/js_api_fetch.asp)
- [JavaScript `async` ja `await`-opas](https://developer.mozilla.org/en-US/docs/Learn/JavaScript/Asynchronous/Async_await)