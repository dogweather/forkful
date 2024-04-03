---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:08:18.799169-07:00
description: "In Ruby, een HTTP-verzoek verzenden met basisverificatie houdt in dat\
  \ je een gebruikersnaam en wachtwoord aan je verzoekheader toevoegt. Programmeurs\
  \ doen\u2026"
lastmod: '2024-03-13T22:44:51.337747-06:00'
model: gpt-4-0125-preview
summary: In Ruby, een HTTP-verzoek verzenden met basisverificatie houdt in dat je
  een gebruikersnaam en wachtwoord aan je verzoekheader toevoegt.
title: Een HTTP-verzoek verzenden met basisauthenticatie
weight: 45
---

## Wat & Waarom?

In Ruby, een HTTP-verzoek verzenden met basisverificatie houdt in dat je een gebruikersnaam en wachtwoord aan je verzoekheader toevoegt. Programmeurs doen dit om toegang te krijgen tot bronnen die gebruikersverificatie vereisen.

## Hoe te:

Om een HTTP-verzoek met basisverificatie te verzenden, gebruik je doorgaans de module `Net::HTTP` in Ruby. Hier is een snel voorbeeld:

```Ruby
require 'net/http'
require 'uri'

uri = URI('http://example.com')
gebruikersnaam = 'je_gebruikersnaam'
wachtwoord = 'je_wachtwoord'

verzoek = Net::HTTP::Get.new(uri)
verzoek.basic_auth(gebruikersnaam, wachtwoord)

reactie = Net::HTTP.start(uri.hostname, uri.port) {|http|
  http.request(verzoek)
}

puts reactie.body
```

Als je deze code uitvoert met geldige inloggegevens, zul je de body van de reactie uitgeprint zien. Als de inloggegevens ongeldig zijn, krijg je een foutmelding.

## Diepere Duik

Basisverificatie heeft een lange geschiedenis als deel van webprotocollen, teruggaand tot de vroege RFC's die de werking van het internet definieerden. Het is een eenvoudige methode voor toegangscontrole: de gebruikersnaam en het wachtwoord worden gecodeerd met Base64 en in de HTTP-header doorgegeven.

Echter, basisverificatie verzendt inloggegevens in platte tekst (hoewel gecodeerd), dus het is niet veilig over HTTP. Het is beter om HTTPS te gebruiken om inloggegevens veilig te houden voor nieuwsgierige blikken.

Er zijn veiligere alternatieven zoals OAuth, dat vaak wordt gebruikt voor API-verificatie. OAuth staat gebruikers toe om toegang van derde partijen te autoriseren zonder inloggegevens te delen. Toch blijft basisverificatie in gebruik, vooral voor interne toepassingen en snelle en vuile scripting.

Een detail om op te merken is dat Ruby's `Net::HTTP` Basic Auth niet native behandelt totdat je expliciet de `basic_auth` methode gebruikt. Het is ook cruciaal om mogelijke uitzonderingen en foutresponsen die kunnen resulteren uit het HTTP-verzoek te behandelen.

## Zie Ook

- Ruby standaardbibliotheek `Net::HTTP` documentatie: https://ruby-doc.org/stdlib-3.0.0/libdoc/net/http/rdoc/Net/HTTP.html
- RFC 7617, 'The 'Basic' HTTP Authentication Scheme': https://tools.ietf.org/html/rfc7617
- Een introductie tot OAuth voor authenticatie: https://oauth.net/2/
- Meer over Ruby en HTTP-verzoeken: https://www.rubyguides.com/2019/08/ruby-http-request/
