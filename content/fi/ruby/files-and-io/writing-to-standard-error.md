---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:34:30.317956-07:00
description: "Kirjoittaminen standardivirheeseen (stderr) Rubylla tarkoittaa virheilmoitusten\
  \ tai diagnostiikkatietojen ohjaamista erilliseen ulostulovirtaan, erill\xE4\xE4\
  n\u2026"
lastmod: '2024-03-13T22:44:57.103728-06:00'
model: gpt-4-0125-preview
summary: "Kirjoittaminen standardivirheeseen (stderr) Rubylla tarkoittaa virheilmoitusten\
  \ tai diagnostiikkatietojen ohjaamista erilliseen ulostulovirtaan, erill\xE4\xE4\
  n standardiulostulosta (stdout)."
title: Kirjoittaminen standardivirheeseen
weight: 25
---

## Mikä ja miksi?
Kirjoittaminen standardivirheeseen (stderr) Rubylla tarkoittaa virheilmoitusten tai diagnostiikkatietojen ohjaamista erilliseen ulostulovirtaan, erillään standardiulostulosta (stdout). Ohjelmoijat tekevät näin erottaakseen tavallisen ohjelman ulostulon virheistä ja virheenjäljitystiedoista, mikä helpottaa ongelmien diagnosointia ja lokien jäsentämistä.

## Miten:
Rubyn vakio kirjasto tarjoaa suoraviivaisen tavan kirjoittaa stderr:iin käyttäen `$stderr` tai `STDERR`. Tätä perustoimintoa varten ei tarvita kolmannen osapuolen kirjastoja.

### Yksinkertaisen viestin kirjoittaminen stderr:iin:
```ruby
$stderr.puts "Virhe: Tiedostoa ei löydy."
# Tai vastaavasti
STDERR.puts "Virhe: Tiedostoa ei löydy."
```
Näyte ulostulo (stderr:iin):
```
Virhe: Tiedostoa ei löydy.
```

### Uudelleenohjataan stderr tiedostoon:
```ruby
File.open('error.log', 'w') do |file|
  STDERR.reopen(file)
  STDERR.puts "Konfiguraation avaaminen epäonnistui."
end
```
Tämä koodinpätkä uudelleenohjaa stderr:n tiedostoon nimeltä `error.log`, ja kaikki myöhemmin kirjoitetut virheet tulostetaan sinne, kunnes ohjelma nollaa stderr:n uudelleenohjauksen tai lopettaa.

### Stderr:n käyttö poikkeusten käsittelyssä:
```ruby
begin
  # Simuloidaan operaatio, joka voi epäonnistua, esim., tiedoston avaaminen
  File.open('olematon_tiedosto.txt')
rescue Exception => e
  STDERR.puts "Poikkeus tapahtui: #{e.message}"
end
```
Näyte ulostulo (stderr:iin):
```
Poikkeus tapahtui: No such file or directory @ rb_sysopen - olematon_tiedosto.txt
```

Vaikka Rubyn sisäänrakennetut metodit stderr:iin kirjoittamiseen riittävät moniin sovelluksiin, monimutkaisempien lokitus tarpeiden osalta saattaa olla harkittava `logger` standardi kirjastoa tai ulkopuolisia jalokiviä, kuten `Log4r`. Nämä tarjoavat mukautettavia lokitusmekanismeja, mukaan lukien vakavuustasot, muotoilut ja kyvyn kirjoittaa useisiin ulostuloihin, mukaan lukien tiedostot, sähköpostit ja muuta.
