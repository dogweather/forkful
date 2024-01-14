---
title:    "Fish Shell: Å starte et nytt prosjekt"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/starting-a-new-project.md"
---

{{< edit_this_page >}}

# Hvorfor

Å starte et nytt prosjekt kan virke skremmende for noen, men med Fish Shell-programmering kan det være enklere enn du tror. Ved å bruke dette kraftige interaktive skallet, kan du enkelt opprette og administrere prosjektene dine mens du samtidig optimaliserer arbeidsflyten din.

# Slik gjør du

For å starte et nytt prosjekt i Fish Shell, åpner du først terminalen og skriver følgende kommando:

```Fish Shell
mkdir mitt-prosjekt
```

Dette vil opprette en ny mappe med navnet "mitt-prosjekt" som vil bli ditt prosjektområde. Deretter navigerer du til denne mappen ved å skrive:

```Fish Shell
cd mitt-prosjekt
```

Nå kan du begynne å kode og bygge prosjektet ditt. Du kan for eksempel opprette en ny fil ved hjelp av kommandoen:

```Fish Shell
touch index.js
```

Dette vil opprette en tom fil med navnet "index.js" som du kan begynne å fylle med koden din. Når du er ferdig med å kode, kan du bruke kommandoen "navn" for å vise koden i terminalen.

```Fish Shell
cat index.js
```

Fish Shell tilbyr også mange nyttige kommandoer for å hjelpe deg med å administrere og organisere prosjektet ditt. For eksempel kan du bruke kommandoen "mv" for å flytte filer, "cp" for å kopiere filer og "rm" for å slette filer.

# Dypdykk

Når du starter et nytt prosjekt, er det viktig å ha en god struktur og organisering fra begynnelsen av. En viktig kommando som kan hjelpe deg med dette er "alias". Dette lar deg opprette en snarvei eller alias for ofte brukte kommandoer. For eksempel kan du opprette et alias for å navigere til prosjektområdet ditt ved å skrive:

```Fish Shell
alias prosjektnavn="cd ~/mitt-prosjekt"
```

Dette vil gjøre det enklere for deg å navigere til prosjektet ditt ved å bare skrive "prosjektnavn" i terminalen.

Det er også en god praksis å bruke virtual environments når du jobber med et prosjekt for å holde det isolert fra andre prosjekter. Fish Shell har en integrert kommando for dette kalt "bass" som gjør det enklere å håndtere og bytte mellom virtual environments.

# Se også

- [Fish Shell dokumentasjon](https://fishshell.com/docs/current/index.html)
- [10 grunner til at du bør bytte til Fish Shell](https://www.shellboost.net/2020/02/14/reasons-to-use-fish-as-your-go-to-shell.html)
- [Enkle triks for å optimalisere Fish Shell-arbeidsflyten din](https://nickjanetakis.com/blog/fish-shell-quick-tips-and-tricks)