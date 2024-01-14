---
title:                "Ruby: Verkkosivun lataaminen"
simple_title:         "Verkkosivun lataaminen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Miksi 

On monia syitä miksi ladata verkkosivu. Joitakin esimerkkejä ovat tiedon kerääminen, analysointi ja tiedon käyttäminen muissa sovelluksissa. Lataaminen voi myös olla hyödyllistä, jos haluat käyttää ulkoisia API-rajapintoja verkkosivuston tietojen saamiseksi.

## Kuinka

Rubylla on useita tapoja ladata verkkosivu. Yksi tapa on käyttää Ruby:n omaa Net::HTTP kirjastoa. Tässä on yksinkertainen esimerkki siitä, kuinka voit ladata sivun ja tulostaa sen HTML-koodin konsoliin:

```Ruby
require 'net/http'

url = URI('https://www.example.com')

response = Net::HTTP.get(url)

puts response.body
```

Tämän koodin tuloste näyttää verkkosivun HTML-koodin ja mahdollistaa sen tallentamisen muuttujaan, jota voit käsitellä myöhemmin.

Voit myös käyttää suosittuja kolmannen osapuolen kirjastoja, kuten Nokogiri tai Mechanize, ladataksesi verkkosivuja Rubylla. Nämä kirjastot tekevät lataamisesta joustavampaa ja pystyvät käsittelemään monimutkaisempia verkkosivuja.

## Syvempi sukellus

Lataaminen ei ole aina yksinkertaista ja saattaa vaatia lisäsäätöjä, kuten määritettyjen otsikoiden lähettämistä tai evästeiden käsittelyä. Net::HTTP kirjasto tarjoaa mahdollisuuden määrittää näitä asetuksia. Esimerkiksi alla olevassa koodissa määritämme otsikkon "User-Agent" ja lähetämme sivulle POST-pyynnön.

```Ruby
require 'net/http'

url = URI('https://www.example.com')

# Luodaan POST-pyyntö
request = Net::HTTP::Post.new(url)
# Määritetään otsikko
request['User-Agent'] = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10.14; rv:65.0) Gecko/20100101 Firefox/65.0'

# Lähetetään pyyntö
response = Net::HTTP.start(url.hostname, url.port, use_ssl: true) do |http|
  http.request(request)
end

puts response.body
```

## Katso myös

- [Ruby:n Net::HTTP kirjasto](https://ruby-doc.org/stdlib-2.7.0/libdoc/net/http/rdoc/Net/HTTP.html)
- [Nokogiri kirjasto](https://nokogiri.org/)
- [Mechanize kirjasto](https://github.com/sparklemotion/mechanize)