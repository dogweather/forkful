---
title:    "TypeScript: Säännöllisten lausekkeiden käyttö"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Miksi käyttää säännöllisiä lausekkeita?

Säännölliset lausekkeet ovat erittäin hyödyllisiä ohjelmoijille, jotka haluavat tehdä tarkkoja hakuja ja muutoksia merkkijonoissa. Ne ovat hyödyllisiä esimerkiksi tiedonkäsittelyssä, tietokantakyselyissä ja minkä tahansa tekstin käsittelyssä, jossa halutaan löytää ja muokata tietyt merkkijonot.

## Kuinka käyttää säännöllisiä lausekkeita TypeScriptissä

Säännölliset lausekkeet on helppo luoda TypeScriptissä käyttämällä RegExp -luokkaa. Alla on esimerkki säännöllisen lausekkeen luomisesta ja sen käyttämisestä merkkijonossa:

```TypeScript
const sana = "Hello World";
const lauseke = /Hello/;
const tulos = sana.match(lauseke);
console.log(tulos); // tulostaa ["Hello"]
```

Tässä esimerkissä luomme säännöllisen lausekkeen, joka etsii merkkijonosta "Hello". Käytämme sitten `.match()` -metodia löytääksemme kaikki esiintymät sanaa "Hello". Tulos tulee olemaan taulukko, joka sisältää kaikki löydetyt esiintymät.

Voit myös käyttää säännöllisiä lausekkeita muiden merkkijonomuokkausmetodien kanssa, kuten `.replace()` ja `.split()`.

```TypeScript
const sana = "Hello World";
const korvattu = sana.replace(/Hello/, "Hei");
console.log(korvattu); // tulostaa "Hei World"
```

## Syvemmälle säännöllisiin lausekkeisiin

Säännölliset lausekkeet ovat paljon monimutkaisempia kuin tässä käsiteltiin. Ne sisältävät joukon sääntöjä ja määrityksiä, jotka auttavat sinua suodattamaan haluamasi tiedot. Voit luoda esimerkiksi säännöllisiä lausekkeita, jotka etsivät tietyn pituisia sanoja, tiettyjä merkkejä tai jopa monimutkaisia mallipohjia. Niitä käyttämällä voit tehdä tarkkoja hakuja ja muutoksia haluamallasi tavalla.

Jos haluat oppia lisää säännöllisistä lausekkeista ja niiden käytöstä TypeScriptissä, suosittelemme lukemaan virallisen dokumentaation [RegExp osion](https://www.typescriptlang.org/docs/handbook/regular-expressions.html).

## Katso myös

- [Ana Bellin opas säännöllisiin lausekkeisiin Pythonissa](https://www.youtube.com/watch?v=K8L6KVGG-7o)
- [Mastering Regular Expressions -kirja](https://www.oreilly.com/library/view/mastering-regular-expressions/0596528124/)
- [Säännölliset lausekkeet Cheat Sheet](https://cheatography.com/davechild/cheat-sheets/regular-expressions/)
- [FreeCodeCampsin opas säännöllisiin lausekkeisiin](https://www.freecodecamp.org/learn/javascript-algorithms-and-data-structures/regular-expressions/)