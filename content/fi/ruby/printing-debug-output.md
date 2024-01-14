---
title:    "Ruby: Tulostettavan virheenkorjaustulosteen tekeminen"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Miksi

Moni kehittäjä joutuu jossain vaiheessa tekemään buginetsintää koodissaan. Tällöin debuggaus eli virheiden etsiminen ja korjaaminen on tärkeää. Yksi hyödyllinen työkalu tässä prosessissa on debug-tulostus eli debug output. Se auttaa selvittämään, mitä koodi tekee ja missä vaiheessa virheitä esiintyy. 

## Miten

Debug-tulostuksen luominen Ruby-ohjelmassa on helppoa. Ensimmäisenä on asetettava halutut tulostustekstit koodiin esimerkiksi puts-metodilla:

```Ruby
puts "Tämä on debug-tulostus"
```

Tämän jälkeen suoritetaan ohjelma ja katsotaan debug-tulostuksen sisältöä. Tulostus löytyy tavallisesti ohjelman suorituksen rinnalla tai sen jälkeen. 

## Syväkatsaus

Debug-tulostus voi sisältää monenlaisia tietoja, kuten muuttujien arvoja ja suoritettuja if-lausekkeita. Tämä auttaa havaitsemaan virheitä ja ymmärtämään, miten koodi toimii. On myös mahdollista käyttää erilaisia debuggaustyökaluja, kuten byebug- tai pry-gemmejä, jotka tarjoavat vielä tarkempia tietoja ohjelman suorituksen aikana.

## Katso myös

- Debuggaus Rubyssa: https://www.ruby-lang.org/fi/documentation/tutorials/ruby-in-your-palm/part-iv/debugging/
- Byebug-gemin käyttöönotto: https://rubygems.org/gems/byebug
- Pry-gemin käyttöönotto: https://rubygems.org/gems/pry