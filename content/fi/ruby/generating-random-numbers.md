---
title:    "Ruby: Satunnaislukujen generointi"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Miksi

Miksi kukaan haluaisi generoida satunnaisia numeroita Ruby-ohjelmalla? Hyvänä esimerkkinä voisi olla tarve testata jotain ohjelmaa tai algoritmia, joka vaatii satunnaisten numeroiden tuottamista.

## Miten

Ruby-ohjelmointikielessä on useita tapoja generoida satunnaisia numeroita. Yksi helpoimmista tavoista on käyttää `rand`-funktiota. Se palauttaa desimaaliluvun väliltä 0-1 ja tätä lukua voidaan muunnella halutulle alueelle. Esimerkiksi seuraavassa koodiesimerkissä generoidaan satunnaisia kokonaislukuja väliltä 1-10 ja tulostetaan ne näytölle:

```Ruby
10.times do
  puts rand(1..10)
end
```
Output:
```
5
3
9
2
6
1
7
10
4
8
```

## Syvempää tietoa

Satunnaisia numeroita voidaan myös generoida antamalla `rand`-funktiolle siemenluvun. Tämä tarkoittaa sitä, että sama siemenluku tuottaa aina saman sarjan satunnaisia numeroita. Tämä voi olla hyödyllistä esimerkiksi testauksessa, jos halutaan varmistaa, että ohjelma toimii samalla tavalla joka kerta.

```Ruby
srand 1234
puts rand
puts rand
puts rand
```
Output:
```
0.1915194503788923
0.6221087710398319
0.4377277390076237
```

## Katso myös

- [Ruby Core -rand documentation](https://ruby-doc.org/core/File.html#method-i-rand)
- [RubyGuides - Generating Random Numbers in Ruby](https://www.rubyguides.com/2019/07/generating-random-numbers/)

[vai näin voisi myös päättää artikkelin]


## Katso myös

Jos haluat oppia lisää satunnaisia numeroita generoimisesta Rubylla, suosittelemme tutustumaan seuraaviin resursseihin:

- [Ruby Core -rand dokumentaatio (englanniksi)](https://ruby-doc.org/core/File.html#method-i-rand)
- [RubyGuides - Satunnaislukujen generointi Rubylla (englanniksi)](https://www.rubyguides.com/2019/07/generating-random-numbers/)