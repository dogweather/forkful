---
title:                "Debuggerin käyttö"
date:                  2024-01-26T03:47:59.489645-07:00
model:                 gpt-4-0125-preview
simple_title:         "Debuggerin käyttö"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/using-a-debugger.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Debuggerin käyttäminen tarkoittaa erikoistyökalujen käyttöä koodin testaamiseen ja diagnostiikkaan. Ohjelmoijat tekevät niin virheiden löytämiseksi, koodivirran ymmärtämiseksi ja varmistaakseen, että heidän koodinsa toimii odotetusti – se on kuin olisi mikroskooppi koodisi aivoille.

## Kuinka:
Kuvittele, että sinulla on pieni ohjelma, joka ei toimi oikein:

```C#
static void Main()
{
    int tulos = Summa(1, 2);
    Console.WriteLine(tulos);
}

static int Summa(int a, int b)
{
    return a + a; // Hups, pitäisi olla a + b
}
```

Käyttämällä Visual Studion debuggeria, aseta katkaisupiste napsauttamalla vasemmassa reunassa kohtaa `return a + a;`. Kun ajat ohjelman (F5:n avulla), suoritus keskeytyy siihen. Vie osoitin muuttujien päälle tarkastaaksesi niiden arvot tai käytä Välitöntä Ikkunaa lausekkeiden arvioimiseen. Näet, että `a` on 1 ja `b` on 2, mutta `a + a` ei ole odotettu summa. Muuta se muotoon `a + b`, jatka suorittamista (F5), ja kas, konsoli tulostaa 3.

## Syväsukellus
Debuggauksen historia ulottuu aina 1940-luvulle, kun oikea bugeihin viittaava hyönteinen (koiperhonen) löydettiin varhaisesta tietokoneesta. Nykypäivän debuggerit, kuten Visual Studiossa oleva, tarjoavat joukon tehokkaita ominaisuuksia, mukaan lukien katkaisupisteet, askel askeleelta suorittaminen, tarkkailuikkunat ja muuta.

Vaihtoehtoja Visual Studion debuggerille ovat avoimen lähdekoodin vaihtoehdot, kuten GDB C-tyylisille kielille tai pdb Pythonille, sekä alustojen väliset IDE:t, kuten JetBrains Rider tai VS Code, jotka tarjoavat debuggaustyökaluja C#:lle ja muille kielille.

Kun sukellat debuggerin toteutukseen, kyse on ohjelmasta, joka liittyy sovelluksesi prosessiin. Se tulkitsee konekieltä, hallitsee muistitilaa ja ohjaa suorituksen kulkua. Tämä on raskasta, mutta ratkaisevan tärkeää tehokkaaseen debuggaukseen, minkä vuoksi debug-tila toimii usein hitaammin kuin release-tila, jossa näitä koukkuja ei ole.

## Katso myös
- [Visual Studio Debugger -dokumentaatio](https://docs.microsoft.com/en-us/visualstudio/debugger/)
- [Debuggausstrategiat](https://www.codeproject.com/Articles/79508/Effective-Exception-Handling-in-Visual-C)
