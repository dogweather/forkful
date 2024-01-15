---
title:                "Päivämäärän muuntaminen merkkijonoksi"
html_title:           "C#: Päivämäärän muuntaminen merkkijonoksi"
simple_title:         "Päivämäärän muuntaminen merkkijonoksi"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Miksi
Mikä on syy, miksi haluaisit muuttaa päivämäärän merkkijonoksi? Ei ole epäilystäkään siitä, että pääset säännöllisesti tilanteeseen, jossa sinun täytyy muuntaa päivämäärä jollakin tavalla esiteltävässä muodossa. Saatat haluta tulostaa sen raporttiin, kirjoittaa sen tietokantaan tai näyttää käyttäjälle. Onneksi C#:ssa on helppo tapa tehdä tämä muunnos yhdestä päivämäärätyypistä toiseen.

## Miten tehdä
Jos haluat muuntaa päivämäärän merkkijonoksi, voit käyttää C#:n DateTime-luokan ToString-menetelmää ja antaa sille haluamasi päivämäärämääritelmän ilmaisun. Alla on esimerkkejä erilaisista päivämäärämalleista ja niiden tulostetuista merkkijonoista:

```C#
// Päivämäärän tulostaminen "dd/MM/yyyy" -muodossa
DateTime today = DateTime.Today;
Console.WriteLine(today.ToString("dd/MM/yyyy")); // Esim. 07/05/2021

// Päivämäärän tulostaminen "dddd, MMMM dd, yyyy" -muodossa
DateTime birthday = new DateTime(1995, 10, 15);
Console.WriteLine(birthday.ToString("dddd, MMMM dd, yyyy")); // Esim. lauantai, lokakuu 15, 1995 
```

Voit myös käyttää DateTime-luokan Parse-menetelmää päinvastaiseen muunnokseen, eli muuttaa merkkijonon päivämääräksi. Alla olevassa esimerkissä haluamme muuntaa merkkijonon "12/31/2021" DateTime-tyypiksi ja tulostaa sen muutetussa muodossa:

```C#
string dateStr = "12/31/2021";
DateTime date = DateTime.Parse(dateStr);
Console.WriteLine(date.ToString("MMMM dd, yyyy")); // Esim. joulukuu 31, 2021
```
 
## Syventävä syventyminen
Päivämäärän muuntaminen merkkijonoksi voi joskus aiheuttaa ongelmia eri kulttuurien ja kielten välillä. Esimerkiksi suomenkielisissä käyttöympäristöissä päivämäärät kirjoitetaan usein muodossa "dd.MM.yyyy", kun taas englanninkielisissä ne ovat tyypillisesti "MM/dd/yyyy". Tästä syystä on tärkeää käyttää CultureInfo-luokkaa määrittämään haluttu kieliasetus ja estää näin mahdolliset virheet päivämäärän muuntamisessa.

Lisäksi DateTime-luokassa on muitakin hyödyllisiä ominaisuuksia, kuten DateDiff-metodi, joka voi laskea aikaeron kahden päivämäärän välillä. Voit myös tarkistaa, onko annettu vuosi karkausvuosi käyttämällä IsLeapYear-metodia.

## Katso myös
- [DateTime-luokka (C#-ohjelmointioppaat)](https://docs.microsoft.com/fi-fi/dotnet/csharp/datetime)
- [CultureInfo-luokka (C#-ohjelmointioppaat)](https://docs.microsoft.com/fi-fi/dotnet/api/system.globalization.cultureinfo?view=net-5.0)
- [Parse-metodi (C#-ohjelmointioppaat)](https://docs.microsoft.com/fi-fi/dotnet/api/system.datetime.parse?view=net-5.0)