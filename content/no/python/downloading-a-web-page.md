---
title:                "Python: Nedlasting av en nettside"
simple_title:         "Nedlasting av en nettside"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/downloading-a-web-page.md"
---

{{< edit_this_page >}}

##Hvorfor 

Å laste ned en nettside ved hjelp av Python kan være en nyttig ferdighet for å skrive skript, hente informasjon eller automatisere oppgaver på nettet.

##Slik gjør du det

For å laste ned en nettside med Python, må du først importere "urllib.request" biblioteket. Dette er standard biblioteket som brukes til å åpne og lese data fra URL-er. 

```Python
import urllib.request
```

Deretter definerer du nettadressen du vil laste ned ved å bruke "urllib.request.urlopen". For eksempel, hvis vi vil laste ned nettsiden "www.norwegian.com", vil koden se slik ut:

```Python
html = urllib.request.urlopen("https://www.norwegian.com")
```

Nå har vi åpnet en tilkobling til nettsiden og lagret innholdet i en variabel som heter "html". For å lese innholdet, kan vi bruke "read()" funksjonen og lagre det i en annen variabel:

```Python
content = html.read()
```

Til slutt, for å lagre innholdet i en fil kan vi bruke "write()" funksjonen sammen med åpningen av en fil. Vi anbefaler å bruke "with" funksjonen, som automatisk lukker filen når den er ferdig. Koden vil se slik ut:

```Python
with open("norwegian.html", "wb") as file:
    file.write(content)
```

Nå vil dette skriptet laste ned nettsiden "www.norwegian.com" og lagre den som en fil kalt "norwegian.html".

##Dypdykk

Hvis du vil lære mer om å laste ned og behandle data fra nettsider, kan du utforske funksjonene i "urllib.request" biblioteket. Det finnes også flere tredjepartsbiblioteker som forenkler prosessen med å laste ned og analysere nettsider, som for eksempel "BeautifulSoup" og "Requests".

## Se også

- "Urllib.request" dokumentasjon (https://docs.python.org/3/library/urllib.request.html)
- "BeautifulSoup" biblioteket (https://www.crummy.com/software/BeautifulSoup/bs4/doc/)
- "Requests" biblioteket (https://requests.readthedocs.io/en/master/)