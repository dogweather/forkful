---
title:    "Ruby: Uuden projektin aloittaminen"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/starting-a-new-project.md"
---

{{< edit_this_page >}}

# Miksi

Oletko joskus halunnut aloittaa uuden ohjelmointiprojektin, mutta et ole ollut varma mistä aloittaa? Ruby on erinomainen ohjelmointikieli monipuoliseen käyttöön ja sen avulla on helppo luoda erilaisia projekteja. Tässä kirjoituksessa käymme läpi, miksi sinun kannattaa harkita uuden projektin aloittamista Rubylla.

# Kuinka aloittaa

Ensimmäiseksi tarvitset tietenkin Rubyn ohjelmointikielen asennettuna tietokoneellesi. Tämän jälkeen voit aloittaa uuden projektisi luomisen.

Vaihtoehto 1: Voit käyttää Ruby'ssa sisäänrakennettua `new`-komentoa uuden projektin luomiseen:


```Ruby
$ new project_name
```

Tämä luo uuden projektikansion nimeltä `project_name` nykyiseen hakemistoosi. Voit sitten siirtyä tähän kansioon ja aloittaa koodaamisen!

Vaihtoehto 2: Voit myös alustaa uuden Git-repositorion ja luoda uuden Ruby-projektin samalla kertaa:

```Ruby
$ git init project_name
$ cd project_name
$ touch app.rb
```
`touch`-komento luo uuden tiedoston `app.rb` projektikansioosi. Tämän jälkeen voit aloittaa koodaamisen.

# Syventyminen

Uuden projektin aloittaminen Rubylla ei kuitenkaan rajoitu vain uuden projektikansion luomiseen. Voit myös käyttää jo olemassa olevia Ruby-työkaluja ja kirjastoja helpottamaan koodaamista ja lisäämään toiminnallisuutta projektiisi.

Jos tarvitset graafista käyttöliittymää, voit esimerkiksi kokeilla Ruby GUI -kirjastoa, joka tarjoaa valmiita työkaluja käyttöliittymän luomiseen Rubylla.

Jos haluat luoda web-sovelluksen, voit käyttää Ruby on Rails -kehyksen tarjoamia työkaluja nopeuttamaan projektisi kehitystä.

# Katso myös

- [Ruby:n virallinen verkkosivusto](https://www.ruby-lang.org/fi/)
- [Ruby GUI -kirjasto](https://www.ruby-toolbox.com/categories/GUI_Frameworks)
- [Ruby on Rails -kehys](https://rubyonrails.org/)