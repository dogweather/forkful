---
title:    "PHP: Å bruke regulære uttrykk"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Hvorfor

Det å kunne bruke regulære uttrykk er en viktig ferdighet for enhver PHP-programmerer. Regulære uttrykk lar deg søke og manipulere tekst på en avansert og effektiv måte. Det sparer deg for mye tid og krefter i å skrive komplekse strengmanipuleringer. 

## Hvordan

For å bruke regulære uttrykk i PHP, må du først bruke funksjonen `preg_match()` eller `preg_match_all()`. La oss si at du ønsker å finne alle forekomster av ordet "telefonnummer" i en tekststreng. Du kan gjøre det ved å bruke følgende kode:

```PHP
$str = "Har du et telefonnummer jeg kan ringe til?";
if (preg_match("/telefonnummer/", $str)) {
   echo "Ja, her er mitt telefonnummer!";
}
```

Ovenstående kode vil skrive ut "Ja, her er mitt telefonnummer!" hvis det finner en match i teksten. Hvis du er interessert i å finne alle forekomster av et ord, så kan du bruke `preg_match_all()` funksjonen. 

Men hva hvis du ønsker å finne en nøye bestemt type tekst, for eksempel et firmas telefonnummer som består av tall med bindestrek mellom hver tredje siffer? Dette er hvor regulære uttrykk skiller seg ut. Du kan bruke såkalte "regular expressions" for å definere hvilket mønster du ønsker å finne. I dette tilfellet kan du bruke følgende kode:

```PHP
$str = "Firmanavnet mitt er XYZ og telefonnummeret mitt er 123-456-789";
if (preg_match("/\d{3}-\d{3}-\d{3}/", $str, $matches)) {
   echo "Firmaets telefonnummer er " . $matches[0];
}
```

Koden vil skrive ut "Firmaets telefonnummer er 123-456-789". La oss gå igjennom hva som skjer her: 

- `/\d{3}-\d{3}-\d{3}/` definerer mønsteret vi ønsker å finne. `\d` betyr alle tall, `{3}` betyr det må være tre siffer etter hverandre, og bindestreken og mellomrommet er der for å definere hvor nummeret skal ha bindestrek.
- `$str` er tekststrengen vi ønsker å søke i.
- `$matches` er en variabel som vil inneholde alle matchene som blir funnet.
- `$matches[0]` er det første matchet som blir funnet (i dette tilfellet, telefonnummeret vårt).

Nå kan vi bruke regulære uttrykk for å finne alle slags kompliserte mønstre og manipulere teksten vår på en effektiv måte.

## Dypdykk

Regulære uttrykk er basert på et sett med spesielle symboler og metakarakterer som definerer forskjellige tekstmønstre. Noen av de vanligste er:

- `^` betyr starten av en tekststreng
- `$` betyr slutten av en tekststreng
- `.` betyr enhver karakter
- `[abc]` betyr en hvilken som helst karakter fra den angitte lista (i dette tilfellet, a, b eller c)
- `[a-z]` betyr en hvilken som helst karakter fra a til z
- `*` betyr null eller flere forekomster av et mønster
- `+` betyr én eller flere forekomster av et mønster
- `?` betyr null eller én forekomst av et mønster
- `()` brukes for å gruppere mønstre sammen

Det er mye å lære når det kommer til regulære uttrykk, og det tar tid å bli komfortabel med å bruke dem. Men når du først forstår konseptet og blir kjent med de vanligste symbolene og metakarakterene, vil du finne ut at det å bruke regulære uttrykk er enkelt og kraftig.

## Se også

- [PHP Manual - Regulære uttrykk](https://www.php.net/manual/en/function.preg-match.php)
- [RegEx