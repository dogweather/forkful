---
title:    "C#: Virheenkorjauksen tulostaminen"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Miksi

On monia syitä, miksi ihmiset turvautuvat debug-tulosteiden tulostamiseen ohjelmointiprojekteissaan. Kaksi yleisintä syytä ovat etsiä virheitä koodista ja seurata ohjelman suoritusta tietyissä osissa.

## Miten

Debug-tulosteiden tulostaminen C#:lla on helppoa ja hyödyntää `System.Diagnostics`-kirjastoa. Voit käyttää `Debug.Write()`- tai `Debug.WriteLine()`-metodia tulostamaan haluamasi tiedot. Tämä esimerkki tulostaa yksinkertaisen viestin konsoliin:

```C#
Debug.Write("Tämä on debug-tuloste.");
```

Tuloste näyttäisi tältä:

```
Tämä on debug-tuloste.
```

Voit myös tulostaa muuttujien arvoja lisäämällä ne metodin parametreihin.

```C#
string nimi = "Matti";
int ika = 30;

Debug.Write("Käyttäjän nimi on ", nimi, " ja ikä on ", ika);
```

Tuloste olisi:

```
Käyttäjän nimi on Matti ja ikä on 30.
```

Debug-tulosteen käyttö voi olla erittäin hyödyllistä monimutkaisten ohjelmien debuggaamisessa, sillä se auttaa sinua ymmärtämään paremmin ohjelmasi toimintaa ja mahdollisia ongelmakohtia. Muista kuitenkin poistaa debug-tulosteet lopullisesta koodista, jotta ne eivät hidasta ohjelmasi suoritusta.

## Syvällinen sukellus

Debug-tulosteiden kanssa voit myös käyttää ehtolauseita, jotta tulosteisiin ei tulostuisi turhia tietoja. Esimerkiksi voit käyttää `#if` ja `#endif` -avainsanoja määrittelemään, milloin tulosteet tulostuvat.

```C#
#if DEBUG
    Debug.Write("Tämä tulostuu vain debug-tilassa.");
#endif
```

Voit myös luoda omia debug-tason symboleja, jotka määrittelevät tulostetaanko debug-tulosteet vai ei. Voit tehdä tämän lisäämällä seuraavanlaisen koodin:

```C#
#define DEBUGMODE // Luo DEBUGMODE-symbolsi

#if DEBUGMODE // Tarkista, onko DEBUGMODE käytössä
    // Tulosta debug-tulosteet tässä
#endif
```

Tämä mahdollistaa vielä tarkemman hallinnan debug-tulosteiden tulostukseen.

## Katso myös

- [Microsoftin Debug-luokan dokumentaatio C#-ohjelmoinnissa](https://docs.microsoft.com/en-us/dotnet/api/system.diagnostics.debug?view=net-5.0)
- [C# Debugging - Asennus ja käyttö](https://www.youtube.com/watch?v=gMklxJXVP4E)
- [Debuggauskommenteista C#-ohjelmoinnissa](https://www.c-sharpcorner.com/article/understaing-code-debug-comment/)