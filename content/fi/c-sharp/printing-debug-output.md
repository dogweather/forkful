---
title:                "C#: Debug-tulostuksen tulostaminen"
simple_title:         "Debug-tulostuksen tulostaminen"
programming_language: "C#"
category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/printing-debug-output.md"
---

{{< edit_this_page >}}

## Miksi

Kun tulet vastaan ongelmia tai virheitä ohjelmointiprojektissasi, on tärkeää pystyä selvittämään, mistä nämä ongelmat johtuvat. Tässä tulee avuksi debug- eli virheenjäljitystulostus. Tulostamalla tarvittavaa tietoa koodin suorituksesta ja muuttujien arvoista, voit paremmin ymmärtää ohjelman toimintaa ja korjata mahdollisia virheitä. Debug-tulostus on siis tärkeä osa tehokasta ohjelmointia.

## Kuinka

C# tarjoaa useita tapoja tulostaa debug-tietoa koodista. Yksi tapa on käyttää Console-luokan Write- tai WriteLine-metodia. Näitä metodeja voi käyttää tulostamaan haluamasi viestin, muuttujien arvoja tai vaikka koko stack trace -tiedon. Alla on esimerkki:

```C#
static void Main(string[] args)
{
    int x = 5;
    int y = 10;
    Console.WriteLine("x:n arvo on " + x);
    Console.WriteLine("y:n arvo on " + y);
    Console.WriteLine("x * y = " + (x * y));
}
```

Tämä koodi tulostaa seuraavan viestin konsoliin:

```
x:n arvo on 5
y:n arvo on 10
x * y = 50
```

Toinen tapa tulostaa debug-tietoa C#-koodissa on käyttää Debug-luokan metodeja. Tämä edellyttää, että olet liittänyt System.Diagnostics -nimisen tilan tiedoston alkuun. Alla on esimerkki Debug-luokan käytöstä:

```C#
static void Main(string[] args)
{
    int i = 5;
    Debug.WriteLine("i:n arvo on " + i);
}
```

Tämä koodi tulostaa seuraavan viestin konsoliin:

```
i:n arvo on 5
```

## Syvällisempi tarkastelu

Debug-tulostuksen avulla voit myös tarkastella ohjelman suoritusaikaisia muuttujien arvoja. Tämä on erityisen hyödyllistä silloin, kun etsit virheitä koodistasi. Esimerkiksi voit käyttää Visual Studio Code -ohjelman debugging-työkaluja nähdäksesi muuttujien arvot step-by-step -tyylisesti koodin suorituksen aikana.

Voit myös lisätä ehtoja tulostamalla debug-tietoa, jolloin tiedät tarkalleen, milloin tieto tulostuu ja milloin ei. Tämä on hyödyllistä esimerkiksi silloin, kun haluat tarkastella tiettyä koodin osaa ja sen vaikutusta ohjelman suoritukseen.

## Katso myös

- [Microsoftin virallinen ohjeistus debug-tulostuksen käyttämisestä C#-koodissa](https://docs.microsoft.com/en-us/visualstudio/debugger/how-to-use-the-debugger-in-visual-studio?view=vs-2019)
- [Ohjeita jälkiselvittelyyn ja debug-tulostuksen käyttöön C#-ohjelmoinnissa](https://www.c-sharpcorner.com/UploadFile/8a67c0/Debugging-in-c-sharp-application/)

Kiitos lukemisesta ja onnea debug-tulostuksen käyttämiseen C#-projekteissasi!