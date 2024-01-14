---
title:    "Ruby: Komentoriviparametrien lukeminen"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Miksi

On monia syitä, miksi sinun kannattaisi lukea komentoriviparametreja Ruby-ohjelmoinnissa. Yksi tärkeimmistä syistä on se, että käyttämällä komentoriviparametreja ohjelmasi voi olla joustavampi ja helpompi käyttää. Tämä tarkoittaa, että käyttäjä voi antaa omia arvoja tietyille parametreille sen sijaan, että ne olisivat kovakoodattuja ohjelmassa.

## Miten

Komentoriviparametrien lukeminen Rubylla on helppoa. Sinun tarvitsee vain käyttää `ARGV`-muuttujaa, joka sisältää kaikki komentorivillä annetut argumentit taulukkona. Voit sitten käsitellä näitä argumentteja kuten mitä tahansa muuta taulukkoa Rubyssa.

```Ruby
# Esimerkki ohjelmasta, joka tulostaa kaikki komentoriviltä annetut argumentit
puts "Komentoriviltä annetut argumentit:"
ARGV.each do |arg|
  puts arg
end
```

Kun suoritat tämän esimerkkiohjelman esimerkiksi komentoriviltä `ruby ohjelma.rb argumentti1 argumentti2`, tulostuu seuraava tulos:

```
Komentoriviltä annetut argumentit:
argumentti1
argumentti2
```

## Syvempi sukellus

Agumenttien lukemisen lisäksi on myös mahdollista antaa komentorivillä valinnaisten argumenttien arvoja. Tämä tehdään yleensä käyttämällä `-` tai `--` merkkejä argumentin nimen edessä ja sen jälkeen annetaan arvo yhteenlaskettuna esimerkiksi `-n 10` tai `--name Ruby`.

```Ruby
# Esimerkki ohjelmasta, joka tulostaa valinnaisen arvon
# Oletusarvoisesti nimeksi asetetaan Ruby, mutta käyttäjä voi antaa oman nimen komentoriviltä
nimi = "Ruby"
ARGV.each do |arg|
  case arg
  when "-n" # -n komentoriviparametri
    nimi = ARGV[ARGV.index(arg) + 1] # Seuraava argumentti on nimi
  when "--name" # --name komentoriviparametri
    nimi = ARGV[ARGV.index(arg) + 1] # Seuraava argumentti on nimi
  end
end
puts "Hei #{nimi}!"
```

Suorittaessa tämän esimerkki ohjelman komennolla `ruby ohjelma.rb -n Juha` tai `ruby ohjelma.rb --name Juha` tulostuu:

```
Hei Juha!
```

## Katso myös

- [ARGV-komentoriviparametrien dokumentaatio Rubyssa](https://ruby-doc.org/docs/keywords/1.9/argv.html)
- [Käyttäjätunnusten läpikäynti Rubyn komentorivistringsillä](https://www.rubyguides.com/2018/05/ruby-argv/)
- [Komentoriviparametrien käyttöönotto Rubyn ohjelmissa](https://www.rubyguides.com/2018/03/ruby-command-line-interface/)