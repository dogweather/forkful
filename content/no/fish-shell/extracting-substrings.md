---
title:    "Fish Shell: Ekstrahering av substringer"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

# Hvorfor

Å trekke ut substrings er en viktig del av å programmere shell scripts i Fish Shell. Det lar deg manipulere og behandle tekstdeler akkurat slik du vil.

# Hvordan

For å trekke ut en substring i Fish Shell, bruker du en kombinasjon av kommandoen 'string' og 'string sub'. For eksempel, hvis du ønsker å trekke ut de første 5 tegnene i en string, skriver du følgende:

``` Fish Shell
set stringen "Hei alle sammen!"
echo $string[1..5]
```

Dette vil gi følgende output:

``` 
Hei a
```

Her bruker vi 'string' kommandoen for å sette variabelen 'stringen' til en string. Deretter bruker vi '[1..5]' for å utheve de første 5 tegnene i stringen. Du kan også bruke denne metoden for å trekke ut en del av en string basert på et bestemt tegn. For eksempel, hvis du vil ha alt etter det andre mellomrommet, kan du gjøre følgende:

``` Fish Shell
set stringen "Hei alle sammen!"
echo $string[(string sub -i " " $string)+1..-1]
```

Dette vil gi følgende output:

``` 
alle sammen!
```

Her bruker vi 'string sub' kommandoen for å finne indeksen av det andre mellomrommet i stringen, og deretter bruker vi denne indeksen som startpunkt for å utheve resten av stringen ved å bruke '-1' som endepunkt for å inkludere alt etterpå.

# Dypdykk

For å trekke ut substrings, kan du også bruke ulike åpne og lukkende parenteser, som kan hjelpe til med å hente ut deler av en string basert på et bestemt mønster. Her er noen eksempler på hvordan du kan bruke dette:

``` Fish Shell
set stringen "Hei alle sammen!"
echo $string[(string match "*alle" $string)..<starten>] 
```

Dette vil gi følgende output:

``` 
Hei
```

Her bruker vi 'string match' sammen med '*' og 'alle' for å matche alt frem til 'alle' i stringen. Deretter bruker vi '..<starten>' for å utheve alt før dette punktet. 

# Se også

- [Fish Shell dokumentasjon for 'string' kommandoen](https://fishshell.com/docs/current/cmds/string.html)
- [Fish Shell dokumentasjon for 'string sub' kommandoen](https://fishshell.com/docs/current/cmds/string.sub.html)
- [Fish Shell dokumentasjon for 'string match' kommandoen](https://fishshell.com/docs/current/cmds/string.match.html)