---
title:    "Ruby: Tulevaisuuden tai menneen päivämäärän laskeminen"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

# Miksi Laskea Päivämäärä Tulevaisuudessa tai Menneisyydessä?

Monissa ohjelmoinnin projekteissa saattaa olla tarve laskea päivämääriä tulevaisuudessa tai menneisyydessä. Tämä voi olla esimerkiksi tarpeen, kun luodaan laskutusjärjestelmä, joka laskee tulevat maksupäivät, tai käsiteltäessä historiallisia tietoja, kuten esimerkiksi tulevia tapahtumia.

# Kuinka Laskea Päivämäärä Tulevaisuudessa tai Menneisyydessä?

Ruby-kielellä tämä onnistuu helposti käyttämällä Date- ja DateTime-luokkia sekä niiden erilaisia metodeja. Esimerkiksi, jos haluamme laskea päivän tietyn määrän päiviä tulevaisuudessa, voimme käyttää Date-luokan "advance" metodia. Seuraavassa esimerkissä lisäämme nykyiseen päivään 5 päivää ja tulostamme uuden päivämäärän:

```Ruby
päivä = Date.today
uusi_päivä = päivä.advance(days: 5)
puts uusi_päivä
```

Tämä tulostaisi seuraavan päivämäärän:

2020-06-26

Vastaavasti, jos haluamme laskea päivämäärän menneisyydessä, voimme käyttää Date-luokan "days_ago" metodia. Seuraavassa esimerkissä laskemme päivämäärän 5 päivää sitten ja tulostamme sen:

```Ruby
päivä = Date.today
uusi_päivä = päivä.days_ago(5)
puts uusi_päivä
```

Tämä tulostaisi seuraavan päivämäärän:

2020-06-16

# Syvemmälle Päivämäärän Laskemiseen Tulevaisuudessa tai Menneisyydessä

Jos haluat syventyä enemmän päivämäärän laskemiseen tulevaisuudessa tai menneisyydessä Rubyssa, kannattaa tutustua Date- ja DateTime-luokkien erilaisiin metodeihin. Näiden luokkien dokumentaatio löytyy Ruby-kielen virallisilta sivuilta.

## Katso myös

- [Ruby Date-luokan dokumentaatio](https://ruby-doc.org/stdlib-2.7.3/libdoc/date/rdoc/Date.html)
- [Ruby DateTime-luokan dokumentaatio](https://ruby-doc.org/stdlib-2.7.3/libdoc/date/rdoc/DateTime.html)