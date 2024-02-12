---
title:                "Koodin refaktorointi"
aliases: - /fi/c-sharp/refactoring.md
date:                  2024-01-26T01:17:19.709166-07:00
model:                 gpt-4-0125-preview
simple_title:         "Koodin refaktorointi"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/refactoring.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Refaktorointi on prosessi, jossa olemassa olevaa tietokonekoodia uudelleenjärjestetään muuttamatta sen ulkoista käyttäytymistä. Ohjelmoijat tekevät sitä puhdistaakseen koodia, parantaakseen luettavuutta, vähentääkseen monimutkaisuutta ja parantaakseen ylläpidettävyyttä.

## Miten:

Refaktoroidaan yksinkertainen C#-metodi, joka laskee ja tulostaa numeroiden summan taulukosta:

Ennen refaktorointia:
```C#
public class Calculator
{
    public void CalculateSum()
    {
        int[] numbers = { 1, 2, 3, 4, 5 };
        int sum = 0;
        for (int i = 0; i < numbers.Length; i++)
        {
            sum += numbers[i];
        }
        Console.WriteLine("Summa on " + sum);
    }
}
```

Refaktoroinnin jälkeen:
```C#
public class Calculator
{
    private readonly int[] _numbers;

    public Calculator(int[] numbers)
    {
        _numbers = numbers;
    }

    public int CalculateSum()
    {
        return _numbers.Sum();
    }

    public void DisplaySum()
    {
        Console.WriteLine($"Summa on {CalculateSum()}");
    }
}

// Käyttö:
var calculator = new Calculator(new[] { 1, 2, 3, 4, 5 });
calculator.DisplaySum();
```

Refaktoroinnilla olemme erottaneet huolenaiheet, tehneet `Calculator`-luokasta joustavamman sallimalla sen ottaa käyttöön minkä tahansa numerotaulukon, ja hyödyntäneet LINQ:ta tehdäksemme summan laskennasta tiiviimmän.

## Syväsukellus

Refaktoroinnin juuret ovat smalltalk-ohjelmointiyhteisössä, ja sen teki suosituksi 1990-luvulla Martin Fowlerin kirja "Refactoring: Improving the Design of Existing Code". Vuosien varrella siitä on tullut olennainen osa ketteriä menetelmiä ja hyviä koodauskäytäntöjä.

Refaktoroinnissa on erilaisia lähestymistapoja, kuten Red-Green-Refactor testivetävässä kehityksessä (TDD). Se varmistaa, että refaktorointi ei tuo mukanaan bugeja aloittamalla epäonnistuneella testillä, saamalla sen onnistumaan ja sitten siivoamalla koodin.

Refaktorointia toteutettaessa on elintärkeää olla kattava testisarja varmistamassa, ettei toiminnallisuus rikkoonnu prosessin aikana. Automaattiset refaktorointityökalut, kuten C#:lle tarkoitettu ReSharper, voivat myös auttaa tässä prosessissa tarjoten turvallisia tapoja muuttaa koodirakenteita. Työkalujen tulisi kuitenkin olla täydentäviä syvälle juurtuneelle koodikannan ja koodausperiaatteiden ymmärrykselle.

## Katso myös

- Martin Fowlerin perusteos refaktoroinnista: [Refactoring: Improving the Design of Existing Code](https://martinfowler.com/books/refactoring.html)
- Microsoftin opas refaktoroinnista Visual Studio:ssa: [Refactoring (C#)](https://docs.microsoft.com/en-us/visualstudio/ide/refactoring-in-visual-studio?view=vs-2022)
- Yksityiskohtainen katsaus refaktorointimalleihin esimerkein: [SourceMaking Refactoring](https://sourcemaking.com/refactoring)
