---
title:    "Ruby: Vertaamalla kahta päivämäärää."
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Miksi

Vertailu on tärkeä osa ohjelmointia ja vertailu kahden päivämäärän välillä on erityisen hyödyllistä, kun haluat tarkistaa esimerkiksi onko jokin tapahtuma tapahtunut ennen tiettyä päivämäärää tai haluat järjestää tapahtumat aikajärjestykseen.

## Miten

Vertailu kahden päivämäärän välillä on helppoa Rubyssa. Voit käyttää `Date`-luokkaa ja sen `<=` ja `>` operaattoreita. Voit myös käyttää `Compare` moduulia ja sen `compare` metodia. Katso esimerkkikoodi alla:

```Ruby
# Luodaan kaksi päivämäärää
tänään = Date.today
huomenna = tänään + 1

# Vertailu <= operaattorilla
if tänään <= huomenna
 puts "#{tänään} on ennen #{huomenna}"
end

# Vertailu > operaattorilla
if huomenna > tänään
 puts "#{huomenna} on myöhemmin kuin #{tänään}"
end

# Käyttämällä Compare moduulia
if Compare.compare(tänään, huomenna) == -1
  puts "#{tänään} on edeltävä päivämäärä #{huomenna}"
end
```

Tämä koodi tuottaa seuraavan tulosteen:

``` 
2021-06-09 on ennen 2021-06-10
2021-06-10 on myöhemmin kuin 2021-06-09
2021-06-09 on edeltävä päivämäärä 2021-06-10
```

## Syväsukellus

Vaikka päivämäärän vertailu Rubyssa on helppoa, on tärkeää olla tietoinen muutamasta asiasta. Esimerkiksi, jos vertaat eri aikavyöhykkeillä olevia päivämääriä, saatat saada odottamattomia tuloksia. Tämä johtuu siitä, että päivämäärä luokka tallentaa päivät UTC-aikana ja tekee vertailut sen mukaan.

Toinen ratkaistava asia on vertailun tarkkuus. Jos vertaat ajan kanssa, seurauksena oleva ero on sekunteina. Tämä tarkkuus voi aiheuttaa ongelmia, jos haluat esimerkiksi verrata kahta päivämäärää, jotka ovat samassa kuukaudessa mutta eri päivinä. Tässä tapauksessa on suositeltavaa käyttää `Date` luokan `==` operaattoria.

## Katso myös

- Ruby `Date` luokka: https://ruby-doc.org/core/Date.html
- Ruby `Comparable` moduuli: https://ruby-doc.org/core-2.7.0/Comparable.html
- Aikavyöhykkeet ja UTC: https://www.timeanddate.com/time/time-zones.html