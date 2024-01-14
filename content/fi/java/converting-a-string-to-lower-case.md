---
title:    "Java: Merkkijonon muuttaminen pienaakkosiksi"
keywords: ["Java"]
---

{{< edit_this_page >}}

# Miksi

Miksi yksilö haluaisi muuntaa merkkijonon pieniksi kirjaimiksi? On useita syitä, miksi tämä toiminto voisi olla hyödyllinen. Yksi yleinen käyttötapaus on tekstin syöttäminen käyttäjältä, jolloin on tärkeää vertailla syötettyä tekstiä ilman, että kirjainkoolla on merkitystä. Toisinaan myös vertailun helpottamiseksi tai käyttöliittymän kauneuden vuoksi halutaan muuntaa merkkijono pieniksi kirjaimiksi.

# Kuinka tehdä se

Koodin alla on esimerkkejä siitä, kuinka muuntaa merkkijono pieniksi kirjaimiksi Java-ohjelmointikielellä.

```Java
// Esimerkki 1: Yksinkertainen tapa käyttää String-luokan sisäänrakennettua toimintoa
String alkuperainenMerkkijono = "TäMÄ onEsimerkki";
String muunnettuMerkkijono = alkuperainenMerkkijono.toLowerCase();
System.out.println(muunnettuMerkkijono); // tulostaa "tämä onesimerkki"

// Esimerkki 2: Käyttäen Character-luokkaa ja for-silmukkaa
String alkuperainenMerkkijono = "vInkkiKonetEknOopy";
char[] merkkijonoTaulukko = alkuperainenMerkkijono.toCharArray();
for (int i = 0; i < merkkijonoTaulukko.length; i++) {
    merkkijonoTaulukko[i] = Character.toLowerCase(merkkijonoTaulukko[i]);
}
String muunnettuMerkkijono = new String(merkkijonoTaulukko);
System.out.println(muunnettuMerkkijono); // tulostaa "vinkkikoneteknoopy"
```

## Syöte ja tulostus

| Alkuperäinen merkkijono | Muunnettu merkkijono |
| --- | --- |
| "TäMÄ onEsimerkki" | "tämä onesimerkki" |
| "vInkkiKonetEknOopy" | "vinkkikoneteknoopy" |

# Syväsukellus

Merkkijonon muuntaminen pieniksi kirjaimiksi on mahdollista myös muilla tavoilla, kuten käyttämällä regular expressionia tai erilaisia kirjastoja. On myös hyvä huomata, että tämä toiminto ei välttämättä tue kaikkia erilaisia kielimerkkejä tai aakkosia. Tästä syystä on tärkeää testata koodia muuntamaan myös erilaisia merkkijonoja ja varmistaa, että toiminto toimii oikein kaikissa tapauksissa.

# Katso myös

- [Java String-luokka](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Java Character-luokka](https://docs.oracle.com/javase/8/docs/api/java/lang/Character.html)
- [Regular expression in Java](https://docs.oracle.com/javase/8/docs/api/java/util/regex/Pattern.html)