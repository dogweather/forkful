---
title:                "Debug-tulosteen tulostaminen"
html_title:           "Bash: Debug-tulosteen tulostaminen"
simple_title:         "Debug-tulosteen tulostaminen"
programming_language: "Python"
category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/printing-debug-output.md"
---

{{< edit_this_page >}}

# Tervetuloa Python-ohjelmoijan matkalle!

Python on yksi suosituimmista ohjelmointikielistä. Sen helppokäyttöisyys, selkeys ja monikäyttöisyys tekevät siitä ihanteellisen kielen aloittelijoille ja kokeneille ohjelmoijille.

***
## Mitä & Miksi?

"Debug-tulosteen" (debug output) tulostaminen on tekniikka jolla näemme ohjelmakoodissamme toimivuuden reaaliajassa. Se on ohjelmoijan tehokas työkalu, sillä se auttaa löytämään ja korjaamaan virheitä helpommin ja nopeammin.

## Miten:

Pythonissa debug-tulosteen tulostaminen on helppoa.

```Python
# Esimerkkikodeja
def laske_summa(a, b):
    summa = a + b
    print(f"Lasketaan: {a} + {b} = {summa}")  # Debug-tulostus
    return summa
```

Tämä ohjelma tulostaa: "Lasketaan: 5 + 3 = 8", kun sitä kutsutaan funktiolla `laske_summa(5, 3)`.

## Syvennys

Debug-tulosteen tulostaminen on historiansa aikana kehittynyt huomattavasti. Alunperin debug-tieto tulostettiin purkkikoodaajien aikana suoraan näytölle tai paperille. Nykyaikana monet ohjelmointikielet, kuten Python, tukevat sisäänrakennettuja debuggaustyökaluja.

Vaihtoehtoina debug-tulostuksen tulostamiselle Pythonissa on esimerkiksi `logging`-kirjasto. Se tarjoaa tarkemman hallinnan tulostukselle, kuten tason asettaminen (esim. INFO, DEBUG, ERROR) ja tulosteen ohjaaminen tiedostoihin. Käyttämällä `logging`-kirjastoa voit säilyttää debug-tietosi, vaikka tuotantoversiossa tulostukset on poistettu.

Pythonin `print`-funktion kautta voidaan tulostaa debug-tekstiä lähes mihin tahansa, mukaan lukien konsoli tai tiedosto. Sen tuominen on helppoa ja nopeatapaista.

## Katso myös:

1. [Pythonin virallinen dokumentaatio](https://docs.python.org/3/)
2. [Logging in Python](https://realpython.com/python-logging/)
3. [Making use of Python logging](https://www.pylenin.com/blogs/python-logging-guide/) 

***

Onnea matkaan Python-maailmassa! Muista, virheiden löytäminen on osa oppimisprosessia. Niiden korjaaminen tekee sinusta paremman ohjelmoijan.