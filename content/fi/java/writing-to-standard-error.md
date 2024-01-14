---
title:    "Java: Kirjoittaminen standardivirheelle"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/java/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Miksi Kirjoittaa Vakiorajauksen?

Kirjoittaminen vakiorajaukseen eli standard erroon on tärkeä osa Java-ohjelmoinnin prosessia. Se antaa sinulle mahdollisuuden havaita ja korjata virheitä, jotka muuten voivat jäädä huomaamatta. Se myös auttaa sinua ymmärtämään paremmin koodisi toimintaa ja parantamaan sen suorituskykyä.

## Kuinka Kirjoittaa Vakiorajaukseen

Kirjoittaminen vakiorajaukseen on helppoa Java-koodin avulla. Sinun tarvitsee vain käyttää `System.err` -muuttujaa ja `println()` -metodia tulostamaan haluamasi virhetiedot. Esimerkiksi:

```Java
public static void main(String[] args) { 
   System.err.println("Virhe: Et saa jakaa lukua nollalla!"); 
}
```
Tämä koodi tulostaa virheen viestin vakiorajauksena, joka näyttää tältä:

```
Virhe: Et saa jakaa lukua nollalla!
```

Voit myös käyttää `System.err` -muuttujaa yhdessä poikkeusten kanssa, jolloin vakiorajaukseen tulostetaan tarkempi virheen kuvaus. Esimerkiksi:

```Java
public static void divideNumbers(int a, int b) { 
   if(b == 0) { 
       throw new ArithmeticException("Et saa jakaa lukua nollalla!"); 
   } 
   System.out.println(a/b); 
}

public static void main(String[] args) { 
   try { 
       divideNumbers(9, 0); 
   } catch (ArithmeticException e) { 
       System.err.println("Virhe: " + e.getMessage()); 
   } 
}
```

Tämä koodi tulostaa vakiorajaukseen seuraavan virheen viestin:

```
Virhe: Et saa jakaa lukua nollalla!
```

## Syvällinen Sukellus Vakiorajaukseen

Vakiorajauksen käyttäminen auttaa sinua havaitsemaan ja hallitsemaan virheitä koodissasi. Yleisimpiä vakiorajauksessa käytettyjä luokkia ovat `System.out` ja `System.err`. Näillä luokilla on kyky ohjata tulostukset eri virtauksiin, joten jos haluat esimerkiksi tulostaa virheen viestin tiedostoon, voit käyttää `System.err` -luokkaa.

Poikkeukset ovat myös tärkeä osa vakiorajauksen käyttöä, sillä ne auttavat sinua ymmärtämään mistä virheistä koodissasi johtuu ja miten ne voidaan korjata. Hyvä käytäntö on myös käsitellä poikkeukset koodissa, jotta ohjelmasi ei kaadu virhetilanteissa.

## Katso Myös

- [Java Documentation: System Class](https://docs.oracle.com/javase/8/docs/api/java/lang/System.html)
- [Java Documentation: Standard Error](https://docs.oracle.com/javase/8/docs/api/java/lang/System.html#error)
- [Tutorialspoint: Java - Writing to Standard Error](https://www.tutorialspoint.com/java/java_writing_to_standard_error.htm)