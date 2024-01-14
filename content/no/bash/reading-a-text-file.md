---
title:                "Bash: Lesing av en tekstfil"
programming_language: "Bash"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Hvorfor

Hvorfor bør du lese denne bloggposten om Bash programmering? Det korte svaret er fordi det er en nyttig og kraftig ferdighet å ha, spesielt for systemadministratorer og utviklere. Bash er et kommandolinjeverktøy som brukes til å automatisere oppgaver og behandle store mengder tekstfiler effektivt. Ved å lære å lese en tekstfil med Bash, vil du kunne håndtere og manipulere data på en effektiv måte.

# Hvordan gjøre det

For å kunne lese en tekstfil med Bash, trenger du noen grunnleggende kommandoer og syntaks. Her er et eksempel på hvordan du kan lese en tekstfil med Bash:

```Bash
#!/bin/bash
while read line
do
    echo $line
done < tekstfil.txt
```

La oss bryte dette ned. Først starter vi med å inkludere "shebang" linjen, som forteller systemet at dette er et Bash-skript. Deretter bruker vi en løkke, "while", som vil iterere over hver linje i tekstfilen. Inne i løkken bruker vi "echo" kommandoen til å skrive ut hver linje. Til slutt bruker vi "<" operatøren til å lese data fra tekstfilen og sende den til løkken.

Etter å ha kjørt dette skriptet, vil du kunne se hver linje i tekstfilen skrevet ut i terminalen. Dette er en enkel, men nyttig måte å lese en tekstfil på med Bash.

# Dypdykk

For å forstå mer av hva som faktisk skjer når vi leser en tekstfil med Bash, må vi gå litt dypere. Når du bruker "<" operatøren, kalles det for en "redirigeringsoperatør". Dette betyr at dataene fra tekstfilen blir sendt til "stdin" (standard inngang) som blir lest av løkken vår. Løkken vil lese hver linje fra "stdin" til variabelen "line", og deretter utføre de nødvendige handlingene.

En annen viktig ting å merke seg er bruken av "while" løkken. Mens den leser hver linje fra tekstfilen, vil den også sjekke om det er flere linjer igjen. Hvis det ikke er flere linjer, vil den stoppe.

# Se også

For å lære mer om Bash programmering og tekstbehandling, sjekk ut disse ressursene:

- [Bash Cheat Sheet (Bash kortkommandoer) by Devhints](https://devhints.io/bash)
- [How to Read a File Line by Line in Bash (Hvordan lese en fil linje for linje med Bash) by Linuxize](https://linuxize.com/post/bash-read-file/)
- [The Linux Command Line (Linux kommandolinjen) by William E. Shotts, Jr.](http://linuxcommand.org/tlcl.php)