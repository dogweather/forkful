---
title:                "Merkkijonojen yhdistely"
html_title:           "C++: Merkkijonojen yhdistely"
simple_title:         "Merkkijonojen yhdistely"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Miksi

Joskus ohjelmoinnissa on tarpeen yhdistää kaksi tai useampia merkkijonoja yhdeksi. Tämä voi esimerkiksi olla hyödyllistä tekstiä käsiteltäessä tai tiedostoja luodessa. Tässä artikkelissa opit, miten voit yhdistää merkkijonoja C++:ssa.

## Kuinka

Yhdistäminen tapahtuu käyttämällä "+" -operaattoria. Alla on esimerkkejä käytöstä:

```C++
// Yhdistetään kaksi merkkijonoa ja tulostetaan tulos
string etunimi = "Matti";
string sukunimi = "Meikäläinen";
cout << etunimi + sukunimi << endl; // Tulostaa "Matti Meikäläinen"

// Voit myös yhdistää useita merkkijonoja
string lause = "Hei, olen ";
string kokonimi = "Matti Meikäläinen";
cout << lause + kokonimi + "!" << endl; // Tulostaa "Hei, olen Matti Meikäläinen!"

// Voit myös yhdistää merkkijonon ja luvun
string sana = "Tämän ohjelman koodirivejä: ";
int koodirivit = 50;
cout << sana + to_string(koodirivit) << endl; // Tulostaa "Tämän ohjelman koodirivejä: 50"
```

### Huomioitavaa

Muista, että yhdistettävien merkkijonojen tulee olla saman tyyppisiä. Esimerkiksi etunimelle ei voi yhdistää numeroa.

```C++
// Tämä aiheuttaisi virheen
string etunimi = "Matti";
int ikä = 30;
cout << etunimi + ikä << endl;
```

## Syvemmällä

C++:n ```string```-luokassa on myös append-metodi, jota voidaan käyttää merkkijonojen yhdistämiseen. Se toimii samalla tavalla kuin "+" -operaattori.

```C++
// Esimerkki append-metodin käytöstä
// Tulostaa "Hei, olen Matti Meikäläinen!"
string lause = "Hei, olen ";
string kokonimi = "Matti Meikäläinen";
lause.append(kokonimi);
cout << lause << endl;
```

Voit myös yhdistää merkkijonoja vektorin avulla. Tästä on hyötyä, jos haluat yhdistää suuremman määrän merkkijonoja.

```C++
// Esimerkki merkkijonojen yhdistämisestä vektorin avulla
// Tulostaa "Matti Meikäläinen, 30 vuotta, Helsinki"
vector<string> tiedot = {"Matti Meikäläinen", "30 vuotta,", "Helsinki"};
string kokonimi = "";
for (string tieto : tiedot) {
    kokonimi += tieto + " ";
}
cout << kokonimi << endl;
```

## Katso myös

- [C++ tietotyypit](https://www.tutorialspoint.com/cplusplus/cpp_data_types.htm)
- [C++ string-luokka](https://www.programiz.com/cpp-programming/string)