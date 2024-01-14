---
title:    "Java: Virheenkorjaustulosteen tulostaminen"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/java/printing-debug-output.md"
---

{{< edit_this_page >}}

## Miksi
Kun työskentelet ohjelmointitehtävien parissa, voi olla hyödyllistä käyttää debuggausta tulostamaan tietoja ohjelman suorituksen aikana. Tämä auttaa sinua havaitsemaan mahdollisia virheitä ja tarkistamaan, mitä koodisi tekee vaiheittain.

## Miten
Debuggauksen tulostaminen Java-koodissa on helppoa. Käytämme siihen ```System.out.println()``` -metodia, joka tulostaa haluamamme tiedot konsolille. Seuraavassa esimerkissä näytämme, kuinka voit tulostaa tekstiä ja muuttujan arvon konsolille:

```Java
String nimi = "Juha";
System.out.println("Tervehdys " + nimi);
```
Tulostus: Tervehdys Juha

Voimme myös tulostaa muuttujien arvoja ja laskutoimituksia käyttämällä debuggausta. Esimerkiksi:

```Java
int a = 5;
int b = 3;
System.out.println("Summa: " + (a + b));
```
Tulostus: Summa: 8

Voit myös käyttää debuggausta tarkistamaan ehtoja ja if-lauseiden toimintaa. Alla olevassa esimerkissä tulostamme tekstin vain, jos ehto täyttyy:

```Java
int ikä = 18;
if(ikä >= 18) {
    System.out.println("Voit äänestää!");
}
```
Tulostus: Voit äänestää!

## Syventyvä tieto
On tärkeää huomata, että debuggausta käytetään vain kehitysvaiheessa ja se poistetaan ennen kuin ohjelma julkaistaan. Koodi, jossa on debug-tulostuksia, voi hidastaa ohjelman suoritusta ja aiheuttaa turhaa tietoliikennettä. Siksi on tärkeää poistaa kaikki debug-tulostukset ennen ohjelman julkaisemista.

On myös erittäin suositeltavaa käyttää kirjastoa, kuten Log4j, debug-tulostusten hallitsemiseen ja poistamiseen helposti. Näin voit helposti ottaa debuggaustulostukset käyttöön tai poistaa ne halutessasi, ilman että sinun tarvitsee muuttaa koodia.

## Katso myös
- [Java Debugging Tutorial](https://www.baeldung.com/java-debugging)
- [How to Debug Java Code in Eclipse](https://www.guru99.com/debugging-in-java-eclipse-tutorial.html)
- [Log4j Documentation](https://logging.apache.org/log4j/2.x/manual/index.html)