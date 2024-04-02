---
date: 2024-01-26 01:07:09.093186-07:00
description: "Lokitus on periaatteessa tapahtumien tallentamisen prosessi, jotka tapahtuvat\
  \ ohjelmiston sis\xE4ll\xE4. Ohjelmoijat lokittavat n\xE4it\xE4 tapahtumia ker\xE4\
  t\xE4kseen\u2026"
lastmod: '2024-03-13T22:44:56.451470-06:00'
model: gpt-4-1106-preview
summary: "Lokitus on periaatteessa tapahtumien tallentamisen prosessi, jotka tapahtuvat\
  \ ohjelmiston sis\xE4ll\xE4. Ohjelmoijat lokittavat n\xE4it\xE4 tapahtumia ker\xE4\
  t\xE4kseen\u2026"
title: Lokitus
weight: 17
---

## Mikä & Miksi?
Lokitus on periaatteessa tapahtumien tallentamisen prosessi, jotka tapahtuvat ohjelmiston sisällä. Ohjelmoijat lokittavat näitä tapahtumia kerätäkseen suoritusaikaista tietoa, vianetsintää, järjestelmän käyttäytymisen seurantaa varten, ja luodakseen tarkastuspolun turvallisuuden ja vaatimustenmukaisuuden tarkoituksessa.

## Kuinka:
Tässä on yksinkertainen tapa aloittaa lokitus Javassa käyttäen sisäänrakennettua `java.util.logging`-pakettia.

```java
import java.util.logging.Logger;
import java.util.logging.Level;

public class AppLogging {
    private final static Logger LOGGER = Logger.getLogger(Logger.GLOBAL_LOGGER_NAME);

    public static void main(String[] args) {
        LOGGER.info("Lokitaan INFO-tason viesti");

        try {
            int jako = 10 / 0;
        } catch (ArithmeticException e) {
            LOGGER.log(Level.SEVERE, "Poikkeus tapahtui", e);
        }
    }
}
```

Tämä tulostaisi jotain seuraavalla tavalla:

```
Heinä 03, 2023 2:00:00 IP AppLogging main
TIETO: Lokitaan INFO-tason viesti
Heinä 03, 2023 2:00:00 IP AppLogging main
VAKAVA: Poikkeus tapahtui
java.lang.ArithmeticException: / by zero
    at AppLogging.main(AppLogging.java:10)
```

## Syväsukellus
Lokitus Javassa on kehittynyt paljon. Historiallisesti lokitus oli enemmän ad-hoc, jossa käytettiin järjestelmän ulostuloja ja itsekirjoitettuja mekanismeja. Tarve standardointiin johti lokitus APIeihin kuten `Log4j` ja `SLF4J`. `java.util.logging`-paketti itsessään otettiin käyttöön JDK 1.4:ssä, tarjoten standardoidun tavan lokiviestien kirjaamiseen.

Vaihtoehtoja `java.util.logging` (JUL) -paketille sisältävät Log4j 2:n ja SLF4J:n. Vaikka JUL on rakennettu suoraan Javaan ja näin ollen ei vaadi lisäriippuvuuksia, tarjoavat sekä Log4j 2 että SLF4J kehittyneempiä ominaisuuksia kuten tarkempi kontrolli lokitusasetuksista, asynkroninen lokitus, ja parempi suorituskyky.

Toteutuksen osalta lokitus voi olla joko synkroninen, jossa jokainen lokiviesti käsitellään säikeessä, joka sen synnytti, tai asynkroninen, jossa viestit siirretään erilliseen säikeeseen. Asynkroninen lokitus voi parantaa suorituskykyä mutta tuo mukanaan monimutkaisuutta, sillä täytyy käsitellä rinnakkaisuutta ja varmistaa, että lokiviestit eivät katoa sovelluksen kaatuessa.

## Katso myös
- [Log4j 2](https://logging.apache.org/log4j/2.x/)
- [SLF4J](http://www.slf4j.org/)
- [Oraclen virallinen lokitusyhteenveto](https://docs.oracle.com/javase/8/docs/technotes/guides/logging/overview.html)
- [Ohje java.util.logging -paketista](https://www.vogella.com/tutorials/Logging/article.html)
