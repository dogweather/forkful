---
title:    "C#: Etsiminen ja tekstin korvaaminen"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Miksi: Turhautuminen tekstiin tekemiseen

Tekstin etsiminen ja korvaaminen on yleinen ohjelmoinnin tehtävä, joka voi aiheuttaa turhautumista, varsinkin kun tekstiä on paljon tai se toistuu useassa tiedostossa. Onneksi C#:n avulla tämä tehtävä voidaan automatisoida, säästäen aikaa ja vaivaa.

## Kuinka tehdä: Koodiesimerkkejä ja tulosteita

Tekstin etsiminen ja korvaaminen C#:lla on suhteellisen helppoa käyttämällä "string" -luokan "Replace()" metodia. Esimerkiksi, jos haluat korvata kaikki sanan "kissa" tiedostossa "teksti.txt" sanalla "koira", seuraava koodi näyttää kuinka tämä tehdään:

```C#
string text = File.ReadAllText("teksti.txt");
text = text.Replace("kissa", "koira");
File.WriteAllText("teksti.txt", text);
Console.WriteLine("Teksti korvattu onnistuneesti!");

```

Tulostuu: "Teksti korvattu onnistuneesti!"

Tässä esimerkissä ensin luetaan tiedosto "teksti.txt" ja tallennetaan se muuttujaan "text". Sitten käytetään "Replace()" metodia korvaamaan kaikki esiintymät sanasta "kissa" sanalla "koira" muuttujassa "text". Lopuksi tallennetaan muuttuja takaisin tiedostoon "teksti.txt". Voit käyttää tätä metodia myös muissa tiedostotyypeissä, kuten CSV, XML tai JSON.

Voit myös käyttää "Regex" luokkaa C#:ssa, jos haluat käyttää säännöllisiä lausekkeita tekstiin etsimiseen ja korvaamiseen. Esimerkiksi, alla oleva koodi korvaisi kaikki numerot tekstissä tyhjällä merkkijonolla:

```C#
string text = "12345 HelloWorld";
string pattern = @"[0-9]";
string replacement = "";
Regex rgx = new Regex(pattern);
string result = rgx.Replace(text, replacement);
Console.WriteLine(result);

```

Tulostuu: " HelloWorld".

## Syvällinen tarkastelu: Lisätietoja tekstin etsimisestä ja korvaamisesta

Vaikka "Replace()" metodi on kätevä ja helppokäyttöinen, siihen liittyy joitakin huomioitavia asioita. Ensinnäkin, korvaus tapahtuu vain, jos annettu merkkijono löytyy kokonaisuudessaan tekstistä. Toiseksi, metodi on case-sensitive, eli se ottaa huomioon kirjainten koolla eron. Jos sinun tarvitsee tehdä tapauksetonta korvaamista, voit käyttää "Replace()" metodia yhdessä "ToLower()" tai "ToUpper()" metodien kanssa, jotka muuttavat merkkijonon kirjaimet pieniksi tai suuriksi.

Regex-luokalla on myös monia ominaisuuksia, kuten globaalin tapauksettomuuden määrittäminen, vain tiettyjen merkkien korvaaminen ja säännöllisten lausekkeiden käyttäminen tarkempaan hakuun ja korvaamiseen.

## Katso myös: Lisää resursseja tekstien etsimisestä ja korvaamisesta C#:lla

- [Microsoftin dokumentaatio Replace() metodista](https://docs.microsoft.com/en-us/dotnet/api/system.string.replace?view=netcore-3.1)
- [Microsoftin dokumentaatio Regex-luokasta](https://docs.microsoft.com/en-us/dotnet/api/system.text.regularexpressions.regex?view=netcore-3.1)
- [C# Regex - Tutorialspoint](https://www.tutorialspoint.com/csharp/csharp_regular_expressions.htm)