---
title:                "Tulostusvirheenkorjauksen tulostaminen"
html_title:           "Python: Tulostusvirheenkorjauksen tulostaminen"
simple_title:         "Tulostusvirheenkorjauksen tulostaminen"
programming_language: "Python"
category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/printing-debug-output.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?

Tulostettavan debug-tietojen lähettäminen on yksinkertainen ja tehokas tapa tarkastella, mitä ohjelma tekee sen suorituksen aikana. Tämä auttaa ohjelmoijia korjaamaan mahdollisia virheitä ja löytämään tehokkaampia tapoja toteuttaa koodia.

## Miten:

Käytä print()-funktiota tulostamaan halutut tiedot. Voit lisätä muuttujia tulostettavan tekstin joukkoon käyttämällä kaarisulkeita { } ja varmistamaan, että tiedot näytetään oikeassa muodossa esimerkiksi käyttämällä str()-funktiota.

```Python
# Tämä koodinpätkä tulostaa "Hello, world!" viestin
print("Hello, world!")

# Tämä tulostaa käyttäjän antaman nimen
nimi = input("Anna nimesi: ")
print("Tervetuloa, " + nimi + "!")
```
Tuloste:
```
Hello, world!
Anna nimesi: John
Tervetuloa, John!
```

## Syvältä sukellus:

Debug-tietojen tulostaminen on ollut suosittu tapa koodin tarkasteluun jo vuosien ajan. Se on myös nopea ja helppo tapa tarkistaa muuttujien arvoja ja ohjelman suoritusjärjestystä. Vaihtoehtoisina tapoina voit käyttää esimerkiksi debugger-työkaluja tai loggausta.

Pythonissa on myös muita tapoja tulostaa tietoja, kuten print()-funktion avulla. Tämä on kätevää, sillä se ei muuta ohjelman suoritusnopeutta.

## Katso myös:

- [Pythonin virallinen dokumentaatio tulostamisesta](https://docs.python.org/3/library/functions.html#print)
- [Debuggaus ja virheenkäsittely - GeeksforGeeks](https://www.geeksforgeeks.org/debugging-in-python/)
- [Pythonin debuggerin käyttö](https://realpython.com/python-debugging-pdb/)