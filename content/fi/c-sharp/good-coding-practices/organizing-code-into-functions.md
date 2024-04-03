---
date: 2024-01-26 01:09:44.095505-07:00
description: "Koodin j\xE4senteleminen funktioiksi on kuin LEGO-palikoiden lajitteleminen\
  \ bokseihin\u2014se tekee niiden l\xF6yt\xE4misest\xE4 ja k\xE4yt\xF6st\xE4 helpompaa.\
  \ T\xE4m\xE4 tehd\xE4\xE4n\u2026"
lastmod: '2024-03-13T22:44:56.578817-06:00'
model: gpt-4-1106-preview
summary: "Koodin j\xE4senteleminen funktioiksi on kuin LEGO-palikoiden lajitteleminen\
  \ bokseihin\u2014se tekee niiden l\xF6yt\xE4misest\xE4 ja k\xE4yt\xF6st\xE4 helpompaa."
title: "Koodin j\xE4rjest\xE4minen funktioihin"
weight: 18
---

## Kuinka:
Kuvittele, että sinulla on koodia, joka tulostaa tervehdyksen useita kertoja. Ilman funktioita se on sotkuinen. Funktioilla se on siisti.

```C#
// Ilman funktioita - toistuvaa
Console.WriteLine("Hello, Amy!");
Console.WriteLine("Hello, Bob!");
Console.WriteLine("Hello, Charlie!");

// Funktioilla - siistimpää
void Greet(string name) {
    Console.WriteLine($"Hello, {name}!");
}

Greet("Amy");
Greet("Bob");
Greet("Charlie");
```

Tuloste on sama, mutta toinen versio on paljon siistimpi.

## Syväsukellus
Kauas taakse, assembly-kielen päiviin, hyppäisit eri koodikohtiin GOTO-komennolla—kaoottista ja vaikeasti seurattavaa. Funktiot ovat merkittävä tasoparannus, kuin järjestetyt laatikot työkalupakissa. Vaihtoehtoja? Tietysti. On metodeja, jotka ovat funktioita luokkakontekstissa. Sitten on lambda-lausekkeet ja inline-funktiot nopeita, kertakäyttöisiä tehtäviä varten.

Toteutuksesta—pienet, fokusoituneet funktiot ovat kultaa. Ne ovat helpompia testata ja debugata. Suuret funktiot, joilla on monta vastuuta, voivat muuttua hirviömäisiksi, ansaiten epäilyttävän tittelin "spagettikoodi". Pidä yksi tehtävä per funktio; kiität itseäsi myöhemmin.

## Katso Myös
Lisää funktioista ja parhaista käytännöistä, tutustu:

- Clean Code by Robert C. Martin: Periaatteet pitääksesi funktiosi siistinä.
- Refactoring by Martin Fowler: Tavat parantaa olemassa olevaa koodia.
- Microsoft C# Guide on Methods: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/classes-and-structs/methods
