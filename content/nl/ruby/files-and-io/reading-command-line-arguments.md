---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:05:40.088336-07:00
description: "Het lezen van command-line argumenten in Ruby stelt scripts in staat\
  \ om direct bij het uitvoeren input te krijgen, zoals het configureren van opties\
  \ of\u2026"
lastmod: '2024-03-13T22:44:51.361217-06:00'
model: gpt-4-0125-preview
summary: Het lezen van command-line argumenten in Ruby stelt scripts in staat om direct
  bij het uitvoeren input te krijgen, zoals het configureren van opties of het doorgeven
  van gegevens.
title: Commandoregelargumenten lezen
weight: 23
---

## Hoe te:
Om command-line argumenten te bemachtigen, biedt Ruby een eenvoudige array: `ARGV`. Het bevat alle argumenten die zijn doorgegeven, in de volgorde waarin ze zijn gegeven.

```Ruby
# hello.rb
naam = ARGV[0] || "Wereld"
puts "Hallo, #{naam}!"

# Uitvoeren met: ruby hello.rb Alice
# Uitvoer: Hallo, Alice!
```

Om meerdere argumenten te hanteren:

```Ruby
# greet.rb
naam, deel_van_de_dag = ARGV
puts "Goede #{deel_van_de_dag || 'dag'}, #{naam || 'daar'}!"

# Uitvoeren met: ruby greet.rb Bob Ochtend
# Uitvoer: Goede Ochtend, Bob!
```

Opties maken met een lus:

```Ruby
# options.rb
opties = {}
ARGV.each do |arg|
  sleutel, waarde = arg.split('=')
  opties[sleutel.to_sym] = waarde
end
p opties

# Uitvoeren met: ruby options.rb naam=Tom leeftijd=30
# Uitvoer: {:naam=>"Tom", :leeftijd=>"30"}
```

## Diepgaand
Het lezen van command-line argumenten is een praktijk zo oud als command-line interfaces zelf. Het gaat om het benutten van gebruikersinput zonder GUI – essentieel voor automatisering of bij het uitvoeren van scripts op servers.

Ruby's `ARGV` is niet uniek; veel talen hebben iets vergelijkbaars. Toch leunt Ruby's implementatie op eenvoud en duidelijke syntax - geen gedoe, gewoon een array.

Onder de oppervlakte is `ARGV` gewoon een instantie van `Array` die vooraf is gevuld met de argumenten die na de scriptnaam in de commando-aanroep verschijnen. Ruby zet het op voordat je code zelfs maar wordt uitgevoerd, waardoor het direct klaar is voor gebruik.

Alternatieven? Zeker. Voor complexe behoeften, zoals het parseren van vlaggen (bv. `--verbose` of `-v`), heeft Ruby de klasse `OptionParser` in de standaardbibliotheek. Dit kan meer aan dan `ARGV`, zoals standaardwaarden, automatische typeconversie en het genereren van helpberichten.

Soms wil je gewoon weten of een argument al dan niet is opgegeven, zonder rekening te houden met de waarde. Daarvoor doet `ARGV.include?` de truc.

## Zie Ook
- Een introductie tot `OptionParser`: [https://ruby-doc.org/stdlib-2.7.0/libdoc/optparse/rdoc/OptionParser.html](https://ruby-doc.org/stdlib-2.7.0/libdoc/optparse/rdoc/OptionParser.html)
- Meer over command-line argumenten in Ruby: [https://www.rubyguides.com/2018/12/ruby-argv/](https://www.rubyguides.com/2018/12/ruby-argv/)
