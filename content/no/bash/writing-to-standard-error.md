---
title:    "Bash: Skriver til standardfeil"
keywords: ["Bash"]
---

{{< edit_this_page >}}

##Hvorfor
Å skrive til standardfeil (stderr) er en nyttig ferdighet for enhver som jobber med Bash-programmering. Dette kan hjelpe til med feilsøking og oppdage potensielle problemer i koden din.

##Slik gjør du det
For å skrive til standardfeil i Bash kan du bruke kommandoen `echo` etterfulgt av teksten du ønsker å sende til stderr for eksempel:
```Bash
echo "Feil! Noe gikk galt." >&2 
```
Dette vil sende teksten "Feil! Noe gikk galt." til standardfeil i stedet for standardutgang. Merk at `>&2` blir brukt for å spesifisere at teksten skal sendes til stderr.

En annen måte å skrive til standardfeil på er å bruke `printf`-kommandoen sammen med ">&2" for å sende teksten til standardfeil. Et eksempel på dette er:
```Bash
printf "Feil! %s gikk galt." "En fil" >&2
```

##Dypdykk
Når du skriver til standardfeil, er det viktig å huske på noen viktige punkter:
- Standardfeil brukes vanligvis for feilmeldinger og informasjon om feil som oppstår under kjøring av et Bash-script.
- Det er vanlig praksis å integrere skriving til standardfeil i feilhåndteringen av Bash-skript for å gjøre det enklere å finne og fikse eventuelle problemer.
- Hvis du vil sende både standardutgang og standardfeil til samme fil, kan du bruke `2>&1` etter kommandoen for å kombinere begge.

##Se også
- [En omfattende guide til Bash-programmering på norsk](https://www.computerhope.com/unix/bash/index-nb.htm)
- [En komplett referanse for Bash-kommandoer](https://ss64.com/bash/)
- [En liste over vanlige feil i Bash-programmering og hvordan du kan unngå dem](https://www.linuxjournal.com/article/10950)