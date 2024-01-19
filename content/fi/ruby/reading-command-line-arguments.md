---
title:                "Komentorivin argumenttien lukeminen"
html_title:           "Bash: Komentorivin argumenttien lukeminen"
simple_title:         "Komentorivin argumenttien lukeminen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Komennon line argumenttien lukeminen tarkoittaa sitä, kun ohjelmasi vastaanottaa tietoja käynnistymishetkellä. Tätä tehdään syötteiden automatisoimiseksi tai ohjelmatilan määrittämiseksi.

## Näin se tehdään:

Command line argumentit tuodaan sisään `ARGV`-taulukon kautta Rubyssa. Tässä on esimerkki:

```Ruby
# printing_args.rb
ARGV.each do|a|
  puts "Argumentti: #{a}"
end
```

Jos ajat tämän ohjelman komentoriviltä annaen argumentteja...

```bash
$ ruby printing_args.rb yksi kaksi kolme
```

...saat tulokseksi:

```bash
Argumentti: yksi
Argumentti: kaksi
Argumentti: kolme
```

## Syvempi tutkimus:

- Historiallinen tausta: Command line argumenttien lukeminen on hyvin vanha käytäntö, joka ulottuu 1970-luvulle, kun käyttöjärjestelmät kehittyivät monikäyttöisiksi. 
- Vaihtoehdot: Voit käyttää myös getopt- tai optparse-kirjastoja argumenttien lukemiseksi ja jäsentämiseksi Rubyssa, joka tarjoaa lisää joustavuutta ja vaihtoehtoja.
- Toteutuksen yksityiskohdat: ARGV on erityislaatuinen taulukko, johon Ruby tallentaa command line argumentit. Tämä poikkeaa muiden ohjelmointikielten käyttämistä menetelmistä, jotka vaativat yleensä erillisen funktion argumenttien käytön.

## Katso myös:

1. [ARGV Ruby-dokumentaatio](http://ruby-doc.org/core-2.1.4/ARGF.html)
2. [Option-parserin syntaksi](https://docs.ruby-lang.org/en/latest/libraries/OptParse.html)
3. [Getoptin dokumentaatio](https://docs.ruby-lang.org/en/latest/libraries/Getopt.html)