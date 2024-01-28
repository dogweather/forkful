---
title:                "Lokitus"
date:                  2024-01-26T01:08:25.680327-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lokitus"
programming_language: "Python"
category:             "Python"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/logging.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Lokitusta tarkoittaa sovelluksen tapahtumien tallentamista ohjelman suorituksen aikana, tarjoten jäljityspolkua jälkianalyysiin ja reaaliaikaiseen valvontaan. Ohjelmoijat tekevät sitä, koska se auttaa virheiden jäljityksessä, suorituskyvyn valvonnassa ja käyttäjien toimien seuraamisessa turvallisuuden ja analysoinnin kannalta.

## Kuinka:
Python sisältää sisäänrakennetun moduulin lokitukseen. Tässä on perusasetus:
```Python
import logging

# Lokituksen peruskonfiguraatio
logging.basicConfig(level=logging.INFO)

# Lokitusviestit
logging.debug('Tämä on debug-viesti')
logging.info('Tietoa mitä ohjelmasi juuri teki')
logging.warning('Varoitusviesti')
logging.error('Virhe on tapahtunut')
logging.critical('Ohjelma ei pysty palautumaan!')
```
Kun suoritat tämän koodin, näet seuraavan tulosteen (koska oletustaso on WARNING, debug- ja info-viestejä ei näytetä):
```
WARNING:root:Varoitusviesti
ERROR:root:Virhe on tapahtunut
CRITICAL:root:Ohjelma ei pysty palautumaan!
```
Voit myös asettaa lokituksen kirjoittamaan tiedostoon eikä konsoliin:
```Python
logging.basicConfig(filename='app.log', filemode='w', level=logging.INFO)
```
Nyt lokisi ohjataan 'app.log' -tiedostoon.

## Syväsukellus
Lokitus on ollut olemassa ohjelmoinnin alkuaikoista lähtien, ja järjestelmälokit ovat yksi vanhimmista pysyvän tallennuksen muodoista itse tietoa sisältäviä tiedostoja lukuun ottamatta. Historiasta huolimatta lokituksen pääkäsite on pysynyt käytännössä muuttumattomana, vaikka työkalut ovatkin kehittyneet.

Pythonin `logging`-moduuli on varsin voimakas ja joustava. Se mahdollistaa erilaisten lokitasojen (DEBUG, INFO, WARNING, ERROR, CRITICAL) asettamisen, mikä voi auttaa lokien luokittelussa ja suodattamisessa. Siinä on hierarkkinen lokerojärjestelmä, mikä tarkoittaa, että voit luoda lokeroille vanhempi-lapsi -suhteita ja levittää viestejä ketjussa ylöspäin.

Vaihtoehtoihin kuuluvat kolmannen osapuolen kirjastot kuten Loguru tai structlog, jotka tarjoavat parannettuja ominaisuuksia ja yksinkertaisemman käyttöliittymän kuin sisäinen lokitusmoduuli. Ne voivat tarjota siistimpää tulostetta, parempaa rakenteellisen datan serialisointia ja intuitiivisempia tapoja käsitellä lokikonfiguraatiota.

Toteutuksen osalta on tärkeää, että lokituksen asetus tehdään kerran sovelluksen alussa. Moduulitason konfigurointi suositellaan käyttämällä `logging.getLogger(__name__)`, jotta noudatetaan Pythonin lokitusparhaita käytäntöjä.

Lokituksen ei tulisi merkittävästi vaikuttaa sovelluksen suorituskykyyn normaaliolosuhteissa. Kuitenkin on oltava varovainen sen suhteen, mitä lokitetaan: liian monisanainen lokitus, erityisesti DEBUG-tasoilla, voi hidastaa sovellusta ja nopeasti täyttää lokitiedostojen tallennustilan.

## Katso myös
Lisätietoja Pythonin lokitusmoduulista löydät virallisesta Pythonin lokituskäsikirjasta, jossa on joitakin loistavia esimerkkejä ja parhaita käytäntöjä: https://docs.python.org/3/howto/logging-cookbook.html

Syvällisempää tietoa rakenteellisesta lokituksesta ja siitä, miten se voi auttaa tekemään lokeista informatiivisempia ja helpommin analysoitavia, Loguru on hyvin dokumentoitu: https://loguru.readthedocs.io

Harkitse myös 12-factor app -metodologian tarkastelua, erityisesti lokien osiota modernista näkökulmasta sovelluslokitukseen: https://12factor.net/logs
