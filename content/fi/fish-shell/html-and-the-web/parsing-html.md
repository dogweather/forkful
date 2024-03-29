---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:22.048273-07:00
description: "HTML:n j\xE4sent\xE4minen on tietojen tai datan poimimista HTML-sis\xE4\
  ll\xF6st\xE4, yleinen teht\xE4v\xE4 ty\xF6skennelt\xE4ess\xE4 web-datan kanssa.\
  \ Ohjelmoijat tekev\xE4t t\xE4m\xE4n\u2026"
lastmod: '2024-03-13T22:44:56.992274-06:00'
model: gpt-4-0125-preview
summary: "HTML:n j\xE4sent\xE4minen on tietojen tai datan poimimista HTML-sis\xE4\
  ll\xF6st\xE4, yleinen teht\xE4v\xE4 ty\xF6skennelt\xE4ess\xE4 web-datan kanssa.\
  \ Ohjelmoijat tekev\xE4t t\xE4m\xE4n\u2026"
title: "HTML:n j\xE4sennys"
---

{{< edit_this_page >}}

## Mikä & Miksi?

HTML:n jäsentäminen on tietojen tai datan poimimista HTML-sisällöstä, yleinen tehtävä työskenneltäessä web-datan kanssa. Ohjelmoijat tekevät tämän automatisoidakseen tiedon poiminnan verkkosivustoilta, tehtäviin kuten web-kaavinta, datakaivos tai automatisoidut testaukset.

## Kuinka:

Fish shell ei pääasiassa ole suunniteltu suoraan HTML:n jäsentämiseen. Kuitenkin, se loistaa yhdistämällä yhteen Unix-työkaluja kuten `curl`, `grep`, `sed`, `awk`, tai käyttämällä erikoistuneita työkaluja kuten `pup` tai `beautifulsoup` Python-skriptissä. Alla on esimerkkejä, jotka esittelevät, kuinka näitä työkaluja voi hyödyntää Fish shellin sisällä HTML:n jäsentämiseen.

### Käyttäen `curl` ja `grep`:
HTML-sisällön noutaminen ja linkkejä sisältävien rivien poiminta:

```fish
curl -s https://example.com | grep -oP '(?<=href=")[^"]*'
```

Tuloste:
```
/page1.html
/page2.html
...
```

### Käyttäen `pup` (komentorivityökalu HTML:n jäsentämiseen):

Varmista ensin, että `pup` on asennettu. Sen jälkeen voit käyttää sitä elementtien poimintaan niiden tagien, id:den, luokkien jne. perusteella.

```fish
curl -s https://example.com | pup 'a attr{href}'
```

Tuloste, samankaltainen kuin `grep` esimerkkissä, listaisi `<a>` tagien href attribuutit.

### Python-skriptillä ja `beautifulsoup`:

Vaikka Fish itsessään ei voi jäsentää HTML:ää natiivisti, se integroituu saumattomasti Python-skripteihin. Alla on tiivis esimerkki, joka käyttää Pythonia ja `BeautifulSoup`ia HTML:n jäsentämiseen ja otsikoiden poimintaan. Varmista, että sinulla on `beautifulsoup4` ja `requests` asennettuna Python-ympäristöösi.

**parse_html.fish**

```fish
function parse_html -a url
    python -c "
import sys
import requests
from bs4 import BeautifulSoup

response = requests.get(sys.argv[1])
soup = BeautifulSoup(response.text, 'html.parser')

titles = soup.find_all('title')

for title in titles:
    print(title.get_text())
" $url
end
```

Käyttö:

```fish
parse_html 'https://example.com'
```

Tuloste:
```
Esimerkkialue
```

Jokainen näistä menetelmistä palvelee erilaisia käyttötapauksia ja monimutkaisuuden asteita, yksinkertaisesta komentorivin tekstimanipulaatiosta koko `beautifulsoup`in jäsentämisen voimaan Python-skripteissä. Tarpeidesi ja HTML-rakenteen monimutkaisuuden mukaan voit valita suoraviivaisen Unix-putkilinjan tai voimakkaamman skriptauslähestymistavan.
