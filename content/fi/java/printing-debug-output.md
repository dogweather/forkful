---
title:                "Debug-tulosteen tulostaminen"
html_title:           "Bash: Debug-tulosteen tulostaminen"
simple_title:         "Debug-tulosteen tulostaminen"
programming_language: "Java"
category:             "Java"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/printing-debug-output.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Tulostaminen debug-tiedot on ohjelmointitekniikka, jonka avulla ohjelmoijat voivat tarkastella sovelluksen sisäistä tilaa. Tätä tekniikkaa käytämme, koska se auttaa meitä korjaamaan vikoja nopeammin.

## Miten tehdä:
Java tarjoaa `System.out.println` -komennon, jonka avulla voit tulostaa debug-tiedot. Tässä on yksinkertainen esimerkki:

```Java
public class DebugDemo {
    public static void main(String[] args) {
        int sum = 0;
        for(int i=1; i<=10; i++){
            sum += i;
            System.out.println("Sum after adding " + i + ": " + sum);
        }
    }
}
```
Tämä ohjelma laskee numerot 1-10 ja tulostaa summan jokaisen lisäyksen jälkeen.

## Syvempi sukellus:
Historiallisesti ottaen, debug-tulostuksen käyttö on ollut yksi vanhimmista ja luotettavimmista debuggausmenetelmistä. Modernit IDE:t ja lähdekoodin debuggaustyökalut tarjoavat korkeatasoisempia tapoja debuggaukseen, kuten asettaminen taukokohtia ja seuraaminen muuttujien tilaa ohjelman suorituksen aikana. Kuitenkin, debug-tulostus antaa yksinkertaisen ja tehokkaan tavan hankkia nopeasti yleiskäsitys ohjelman tilasta.

Java käyttää virtuaalikoneen (JVM) standarditulostuskanavaa (standard out, tai sysout) debug-tulostuksen lähettämiseen, johon `System.out.println` kirjoittaa.

On myös olemassa alternativeja `System.out.println` komennolle, kuten loggauskehykset (esim. Log4J, SLF4J), jotka tarjoavat enemmän joustavuutta ja kontrollia viestien tulostamisesta ja tallentamisesta.

## Katso myös:
1. Oracle Java Documentation: [System (Java Platform SE 8 )](https://docs.oracle.com/javase/8/docs/api/java/lang/System.html)
2. Apache Logging Services: [Log4j](https://logging.apache.org/log4j/2.x/)
4. StackOverflow Discussion: [When and why is it appropriate to use System.out.println() for debugging?](https://stackoverflow.com/questions/1778689/when-and-why-is-it-appropriate-to-use-system-out-println-for-debugging)