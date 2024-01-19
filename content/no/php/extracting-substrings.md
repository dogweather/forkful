---
title:                "Utdrag av understrenger"
html_title:           "Bash: Utdrag av understrenger"
simple_title:         "Utdrag av understrenger"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å ekstrahere delstrenger er å isolere en spesifikk del av en tekststreng i programmering for videre bruk. Programmerere gjør dette for å manipulere og analysere data mer effektivt.

## Hvordan gjøre det:

For å ekstrahere delstrenger, bruker vi i PHP ofte `substr() `-funksjonen. Men det er også andre funksjoner tilgjengelig, som `strpos()`. Her er noen eksempler: 

```PHP
<?php
    $streng = "Hei, Verden!";
    echo substr($streng, 4, 6); // Output: ", Ver"
?>

<?php
    $streng = "Jeg elsker PHP!";
    echo substr($streng, -4); // Output: "PHP!"
?>
```
## Dypdykk

1. Historisk kontekst: Delstrengfunksjoner har vært en del av PHP siden versjon 4. Ettersom PHP har utviklet seg, har også funksjonen `substr()` blitt forbedret og optimalisert.

2. Alternativer: Hvis du for eksempel trenger å håndtere multibyte-tegn (som i Unicode), bør du vurdere å bruke `mb_substr() `, som er spesielt designet for dette formålet.

3. Implementasjonsdetaljer: `substr()` tar tre argumenter: (1) opprinnelig streng, (2) startposisjon, og (3) (valgfritt) lengden på den delen du ønsker å utvinne. Hvis det tredje argumentet utelates, returnerer funksjonen resten av strengen fra startposisjonen.

## Se også:

Gå til PHP manuelle sider for mer inngående informasjon og flere eksempler på `substr()` og `mb_substr()`:

- [`substr()` dokumentasjon](https://php.net/substr)
- [`mb_substr()` dokumentasjon](https://php.net/mb_substr)