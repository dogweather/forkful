---
title:    "C++: Testien kirjoittaminen"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Miksi: Miksi harrastaa testien kirjoittamista?

Testien kirjoittaminen on tärkeä osa ohjelmistokehitystä, joka auttaa parantamaan koodin laatua ja vähentämään ohjelmistovirheitä. Testaaminen auttaa myös varmistamaan, että koodi toimii kuten odotetaan ja mahdollistaa sujuvan muutosten tekemisen tulevaisuudessa.

## Miten: Esimerkkejä testien kirjoittamisesta C++:ssa

```C++
#include <iostream>
#include <cassert>

using namespace std;

// Yksinkertainen funktio, joka lisää kaksi numeroa
int sum(int a, int b) {
	return a + b;
}

// Testi, joka varmistaa että summa on oikein laskettu
void testSum() {
	assert(sum(3, 4) == 7);
	assert(sum(10, 20) == 30);
	assert(sum(-5, 10) == 5);
}

// Pääohjelma
int main() {
	testSum();
	cout << "Testit läpäisty onnistuneesti!" << endl;
	return 0;
}
```

Tämän ohjelman tulostus näyttäisi seuraavalta:

```bash
Testit läpäisty onnistuneesti!
```

Tässä yksinkertaisessa esimerkissä käytämme `assert`-funktiota testien kirjoittamiseen. `assert` tarkistaa, että annettu ehto on tosi, ja jos ei ole, ohjelma lopetetaan virheellä. Tämä antaa meille selkeän ja helpon tavan tarkistaa, että koodimme tuottaa halutut tulokset.

## Syvempi sukellus: Testien kirjoittamisen taustaa

Testien kirjoittaminen on tärkeä osa ohjelmistokehitystä, joka auttaa parantamaan koodin laatua ja vähentämään ohjelmistovirheitä. Testien kirjoittaminen ennen itse koodin kirjoittamista auttaa kehittäjää hahmottamaan haluttua toiminnallisuutta ja ehkäisemään mahdollisia virheitä jo ennen koodin kirjoittamista.

Hyvän testikattavuuden saavuttaminen ei kuitenkaan tarkoita pelkästään testien kirjoittamista kaikenkattavasti. On myös tärkeää miettiä, millaisia testejä kannattaa kirjoittaa ja miten niitä kannattaa järjestää. Testien automatisointi on myös tärkeä aspekti, joka säästää aikaa ja vaivaa koodin muuttuessa.

## Katso myös

- [Assert() dokumentaatio (C++)](https://www.cplusplus.com/reference/cassert/assert/)
- [Hyvät testaustavat (C++)](https://www.jetbrains.com/help/clion/unit-testing-tutorial-and-reference.html)
- [Testikattavuus (C++)](https://docs.microsoft.com/en-us/cpp/test/unit-tests-cpp?view=msvc-160)