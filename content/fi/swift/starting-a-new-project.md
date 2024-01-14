---
title:    "Swift: Uuden projektin aloittaminen"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/swift/starting-a-new-project.md"
---

{{< edit_this_page >}}

# Miksi aloittaa uusi projekti

Aloittaa uusi ohjelmointiprojekti voi vaikuttaa pelottavalta ja ylivoimaiselta tehtävältä, mutta todellisuudessa se voi tuoda paljon hyötyä ja oppimiskokemuksia. Uuden projektin aloittamalla voit harjoitella uusia taitoja ja luoda jotain uniikkia ja omaa, jolla voi olla suuri vaikutus muille.

# Miten aloittaa uusi projekti

Ensimmäinen askel uuden Swift-projektin aloittamisessa on avata Xcode ja valita "Create a new Xcode project" -vaihtoehto. Valitse sitten "Single View App" -pohja ja anna projektillesi nimi.

Seuraavaksi voit luoda uuden Swift-tiedoston projektillesi painamalla "Command + N". Tämä luo tyhjän Swift-tiedoston, johon voit alkaa kirjoittaa koodia. Voit myös käyttää valmiita mallipohjia ja kirjastoja helpottaaksesi projektisi aloittamista.

Tässä esimerkissä luomme yksinkertaisen laskimen, joka ottaa käyttäjän syöttämän ensimmäisen numeron ja toisen numeron ja laskee niiden summan. Ensimmäiseksi lisäämme alussa tarvittavat muuttujat ja pyydämme käyttäjältä syötteen:

```Swift
var firstNumber: Int
var secondNumber: Int

print("Anna ensimmäinen numero:")
firstNumber = Int(readLine() ?? "") ?? 0
print("Anna toinen numero:")
secondNumber = Int(readLine() ?? "") ?? 0
```

Seuraavaksi laskemme numeroiden summan ja tulostamme sen konsoliin:

```Swift
let sum = firstNumber + secondNumber
print("Summa on \(sum)")
```

Lopuksi voit testata koodin toimivuuden ajamalla projektia ja antamalla sille erilaisia syötteitä.

# Syvällinen sukellus

Aloittaminen uuden Swift-projektin kanssa ei ole vain pelkkää koodien kirjoittamista, vaan se vaatii myös suunnittelua ja mietiskelyä. Ennen projektin aloittamista on hyvä pohtia, mitä haluat saavuttaa ja mikä on projektillesi tarkoitus. Tämä auttaa sinua luomaan selkeän suunnitelman ja välttämään mahdollisia ongelmia projektin edetessä.

On myös hyvä idea tutustua hyviin ohjelmointikäytäntöihin ja seurata ohjeita, jotta voit luoda laadukkaan ja helposti ylläpidettävän koodin. Kannattaa myös pitää projektisi dokumentointi ajan tasalla ja käyttää versionhallintaa, jotta voit seurata ja palata vanhoihin versioihin tarvittaessa.

# Keksiä myös

[Vinkkejä Swift-projektin aloittamiseen](https://www.raywenderlich.com/5991-swift-tutorial-part-0-expressions-variables-and-constants)

[Swift-oppaat ja tutoriaalit](https://developer.apple.com/documentation/swift)

# Katso myös

[Kuinka oppia Swiftiä nopeasti](https://fi.education.pabili.com/swift)

[Parhaat ohjelmointikäytännöt Swiftillä](https://developer.apple.com/swift/)