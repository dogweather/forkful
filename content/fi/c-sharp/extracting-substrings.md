---
title:                "Alimerkkijonojen poiminta"
html_title:           "Gleam: Alimerkkijonojen poiminta"
simple_title:         "Alimerkkijonojen poiminta"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

# Leikkaa & Opettele: Kuinka otetaan alijonot irti C#-kielellä

## Mitä & Miksi?
Alijonojen poiminta tarkoittaa merkkijonon osan ottamista itsenäiseen käyttöön. Ohjelmoijat tekevät tätä usein käsitelläkseen tai muokatakseen osia suuremmasta tietomassasta.

## Kuinka:
C#-kielellä alijonojen poiminta on helppoa. Pieneen osaan ohjelmaa voi sisällyttää seuraavan toimintalogiikan:

```C#
string lauseessa = "Tervetuloa Suomeen";
string alijono = lauseessa.Substring(0, 11);
Console.WriteLine(alijono);
```

Näytölle tulostuu:

```C#
"Tervetuloa "
```

## Syvä sukellus
Historiallisessa kontekstissa String-luokan Substring-metodi on ollut C#-kielen työkaluista käytössä varhaisesta vaiheesta lähtien, ja se on säilyttänyt paikkansa erittäin hyödyllisenä työkaluna.

Vaihtoehtona Substring-metodille C#-kielellä voidaan käyttää myös juoksukykyä tai LINQ-kyselyä, mutta käytännön tehokkuuden kannalta Substring on useimmiten parempi vaihtoehto.

Substring-metodin toteutuksen osalta, se ottaa kaksi parametria: aloitusindeksin (0-pohjainen) ja pituuden. Se palauttaa uuden merkkijonon, joka alkaa aloitusindeksistä ja jatkuu määritellyn pituuden verran.

## Katso myös
- Microsoftin ohjelmointioppaassa: [Ohjeita merkkijonojen käsittelyyn](https://docs.microsoft.com/fi-fi/dotnet/csharp/programming-guide/strings/) 
- Stack Overflow: [Miten voin leikata merkkijonon C#-kielellä?](https://stackoverflow.com/questions/308629/how-to-use-c-sharp-substring-c-sharp-net-substring-syntax)