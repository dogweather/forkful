---
title:                "Ruby: Kirjoittaminen standardivirheeseen."
programming_language: "Ruby"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Miksi

Ohjelmointi ei aina mene suunnitellusti ja usein törmäämme virheisiin koodin suorituksen aikana. Kirjoittamalla virheviestejä standardivirheeseen voimme auttaa itseämme ja muita koodarinä hyödyntämään näitä virheviestejä ja selventämään ongelman syitä ja ratkaisuja.

## Kuinka

Yksi tapa kirjoittaa standardivirheeseen on käyttämällä Rubyn sisäänrakennettua `warn`-metodia. Tämä metodi ottaa vastaan merkkijonon, joka tulostetaan standardivirheeseen. Tämä voi olla hyödyllistä esimerkiksi silloin, kun haluamme testata, mikä kohta koodissa aiheuttaa ongelman.

```Ruby
5 / 0 # tulostaa virheen " ZeroDivisionError: divided by 0" standardilähtöön
warn "Tarkista jakajaksi laittamasi luku!" # tulostaa viestin standardivirheeseen
```

Voimme myös tallentaa virheviestin muuttujaan ja tulostaa sen standardivirheeseen, mikäli haluamme lisätä siihen enemmän tietoa ongelman syystä. Esimerkiksi:

```Ruby
begin
  5 / 0
rescue ZeroDivisionError => e
  message = "Tarkista jakajaksi laittamasi luku! Virhe: #{ex.message}"
  warn message # tulostaa virheen "Tarkista jakajaksi laittamasi luku! Virhe: divided by 0" standardilähtöön
end
```

## Syvemmälle

Virheiden kirjoittaminen standardivirheeseen voi auttaa myös debuggaamisessa eli ongelman etsimisessä. Voimme esimerkiksi tulostaa muuttujan arvon standardivirheeseen, jolloin näemme helpommin, missä vaiheessa koodissa on ongelma. Esimerkiksi:

```Ruby
i = 10
j = 0
begin
  result = i / j
rescue ZeroDivisionError => e
  message = "Jakojäännös on #{i % j}, ei voida jakaa #{i} luvulla #{j}."
  warn message # tulostaa virheen "Jakojäännös on 0, ei voida jakaa 10 luvulla 0." standardilähtöön
end
```

Virheiden kirjoittaminen standardivirheeseen voi myös auttaa meitä parantamaan koodin laatua ja estämään tulevia virheitä. Kun tiedostamme, millaisia virheitä koodissa voi ilmetä, voimme ennakoida niitä ja lisätä turvaehdotuksia tai virheviestejä, jotka auttavat meitä tai muita kehittäjiä ratkaisemaan ongelman nopeammin.

## Katso myös

- Rubyn virheenkäsittelydokumentaatio: https://ruby-doc.org/core-2.7.1/Exception.html
- Rubyn virheviestit standardivirheessä: https://ruby-doc.org/core-2.7.1/Kernel.html#method-i-warn