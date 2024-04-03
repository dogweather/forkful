---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:59:53.300909-07:00
description: "Hoe: Ruby biedt een eenvoudige manier om bestanden ter plaatse rechtstreeks\
  \ vanaf de commandoregel te bewerken. Met Ruby's `-i`-schakelaar kun je Ruby\u2026"
lastmod: '2024-03-13T22:44:51.333622-06:00'
model: gpt-4-0125-preview
summary: Ruby biedt een eenvoudige manier om bestanden ter plaatse rechtstreeks vanaf
  de commandoregel te bewerken.
title: Bestanden ter plekke bewerken met CLI one-liners
weight: 32
---

## Hoe:
Ruby biedt een eenvoudige manier om bestanden ter plaatse rechtstreeks vanaf de commandoregel te bewerken. Met Ruby's `-i`-schakelaar kun je Ruby opdracht geven rechtstreeks op de opgegeven bestand(en) te werken. Laten we met een paar voorbeelden spelen om te zien hoe dit in het echte leven werkt. Stel je hebt een bestand `greetings.txt` met de volgende inhoud:

```
Hallo, wereld!
Hallo, Ruby!
Hallo, programmeren!
```

En je wilt het woord "Hallo" vervangen door "Hi". Hier is hoe je dat kunt doen:

```Ruby
ruby -i -pe "gsub(/Hallo/, 'Hi')" greetings.txt
```

Na het uitvoeren van dit commando, wordt `greetings.txt` bijgewerkt naar:

```
Hi, wereld!
Hi, Ruby!
Hi, programmeren!
```

Als je je zorgen maakt over het potentieel verpesten van gegevens, heeft Ruby een oplossing voor je. Door een extensie aan de `-i`-schakelaar te bieden, maakt Ruby een back-up voordat de wijzigingen worden uitgevoerd. Bijvoorbeeld:

```Ruby
ruby -i.bak -pe "gsub(/Hallo/, 'Dag')" greetings.txt
```

Nu vind je, samen met je bewerkte `greetings.txt`, een `greetings.txt.bak` in dezelfde map, met de originele inhoud.

## Diep Duiken
De magie van het ter plaatse bewerken van bestanden in Ruby komt voort uit zijn combinatie van Perl-achtige tekstverwerkingsmogelijkheden en de eigen syntactische elegantie van Ruby. Historisch gezien was Perl de voorkeurstaal voor snelle one-liner scripting, vooral voor tekstmanipulatie. Ruby heeft dit paradigma overgenomen, wat zorgt voor krachtige commandoregel scriptingmogelijkheden.

Er bestaan alternatieven voor het ter plaatse bewerken in andere talen, zoals Perl zelf en sed, een stream editor in Unix-systemen. Elk heeft zijn sterke punten - Perl staat bekend om zijn tekstverwerkingskracht terwijl sed ongeëvenaard is in zijn eenvoud voor streambewerkingstaken. Ruby biedt echter een evenwicht, door robuuste tekstmanipulatie te bieden met een leesbaardere en gebruiksvriendelijkere syntaxis, vooral voor degenen die al bekend zijn met Ruby.

Wat de implementatie betreft, werkt de ter-plaatse-bewerking van Ruby door het originele bestand te hernoemen, een nieuw bestand met de originele bestandsnaam te creëren en vervolgens de wijzigingen naar dit nieuwe bestand te schrijven terwijl het uit het hernoemde origineel leest. Deze aanpak zorgt voor de atomiciteit van de operatie; ofwel wordt het hele bestand succesvol verwerkt, of er worden geen wijzigingen aangebracht, wat de integriteit van je gegevens tijdens het bewerkingsproces beschermt. Dit mechanisme, gecombineerd met Ruby's uitzonderingsafhandeling, biedt ook veerkracht tegen onderbrekingen, zoals stroomuitval of het stoppen van processen, en zorgt ervoor dat ten minste de back-up intact blijft.

Samengevat is de ter-plaatse-bestandsbewerking van Ruby een getuigenis van zijn nut als een scriptingtaal, die een mengeling biedt van kracht, eenvoud en elegantie voor tekstmanipulatietaken rechtstreeks vanaf de commandoregel.
