---
title:                "C#: Tiedoston lukeminen"
programming_language: "C#"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Tekstitiedostojen lukeminen on tärkeä ja usein tarvittu taito kaikille ohjelmoijille. Monissa projekteissa joudutaan käsittelemään tekstiä, kuten lokitiedostoja, tietokantojen kenttiä, tai käyttäjän syöttämiä tietoja. Tässä blogipostissa opit lukemaan tekstitiedostoja C# ohjelmointikielellä!

## Kuinka

Jotta voit lukea tekstitiedostoja C# kielellä, sinun täytyy ensin avata tiedosto input stream -komennolla. Tämän jälkeen voit lukea tiedoston sisältöä käyttämällä TextReader luokkaa. Alla olevassa esimerkissä luomme teksti tiedoston nimi “teksti.txt” ja kirjoitamme siihen muutaman rivin tekstiä:

```C#
public static void Main(){
  // Luodaan tekstitiedosto
  File.WriteAllText("teksti.txt", "Tämä on esimerkkitiedosto tekstinlukemista varten.");

  // Avataan tiedosto input stream -komennolla
  FileStream fs = new FileStream("teksti.txt", FileMode.Open);

  // Luodaan TextReader ja luetaan tiedoston sisältöä
  using (TextReader reader = new StreamReader(fs)){
    string line;

    while ((line = reader.ReadLine()) != null){
      Console.WriteLine(line);
    }
  }
}
```

#### Output:
Tämä on esimerkkitiedosto tekstinlukemista varten.

Kuten näet, avasimme tiedoston input stream -komennolla ja luimme sen sisältöä riviltä riville käyttämällä TextReader luokkaa. TextReader luokassa on muitakin hyödyllisiä metodeja tiedoston lukemiseen, kuten Read() ja ReadToEnd(), jotka voit oppia lisää dokumentaatiosta.

## Syväsukellus

Tekstitiedostojen lukeminen ei kuitenkaan rajoitu vain yksittäisiin riveihin. Voit myös käyttää C# kielen säännöllisiä lausekkeita (regular expressions) helpottamaan tiedoston lukemista ja tietojen parseerausta. Tämä säästää paljon aikaa ja vaivaa, varsinkin jos käsiteltävät tiedostot ovat suurempia.

Alla olevassa esimerkissä käytämme Regex luokkaa löytämään ja tulostamaan kaikki sanoja, jotka alkavat isolla kirjaimella tiedostosta “teksti.txt”:

```C#
public static void Main(){
  // Luodaan tekstitiedosto
  File.WriteAllText("teksti.txt", "Tämä on esimerkkitiedosto tekstinlukemista varten.");

  // Avataan tiedosto input stream -komennolla
  using (var sr = new StreamReader("teksti.txt")){
    var regex = new Regex(@"[A-Z]\w+");

    // Käydään läpi tiedoston jokainen rivi
    while(!sr.EndOfStream){
      string line = sr.ReadLine();

      // Etsitään kaikki sanat, jotka alkavat isolla kirjaimella ja tulostetaan ne
      MatchCollection matches = regex.Matches(line);

      foreach (Match match in matches){
        Console.WriteLine(match.Value);
      }
    }
  }
}
```

#### Output:
Tämä
Tiedosto
Lukemista

Tässä esimerkissä käytämme säännöllistä lauseketta [A-Z]\w+, joka etsii kaikki sanat, jotka alkavat isolla kirjaimella (A-Z) ja ovat vähintään yhden merkin pituisia (\w+). Regex luokka tarjoaa paljon eri mahdollisuuksia tiedoston lukemiseen ja datan käsittelyyn, joten kannattaa ottaa selvää siitä lisää dokumentaatiosta.

## Katso myös
- [C# TextReader luokka](https://docs.microsoft.com/en-us/dotnet/api/system.io.textreader?view=