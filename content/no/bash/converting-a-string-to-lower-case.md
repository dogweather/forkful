---
title:    "Bash: Konvertering av streng til små bokstaver"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Hvorfor

I dagens digitale verden er det viktigere enn noen gang å kunne behandle tekst og data på en effektiv og nøyaktig måte. En vanlig programmeringsoppgave er å konvertere en streng (string) til små bokstaver (lower case), og dette kan ha mange fordeler. Det gjør at vi kan sammenligne tekst på en enklere måte, og det gir en mer konsistent og lesbar tekststruktur. I denne bloggposten vil vi se nærmere på hvordan man kan konvertere en streng til små bokstaver i Bash.

## Hvordan

Det finnes flere måter å konvertere en streng til små bokstaver i Bash, men den enkleste og mest effektive metoden er å bruke kommandoen "tr -s '[:upper:]' '[:lower:]'". La oss se på et eksempel:

```Bash
original_string="DETTE ER EN STRENG"
lower_case_string=$(echo $original_string | tr -s '[:upper:]' '[:lower:]')
echo $lower_case_string
```
Output:
```Bash
dette er en streng
```

Her bruker vi først en variabel for å lagre en string med store bokstaver, og deretter bruker vi kommandoen "tr" for å konvertere bokstavene til små. Dette gjøres ved å bruke en "[:upper:]" for å spesifisere at vi vil endre store bokstaver, og en "[:lower:]" for å spesifisere at vi vil endre til små bokstaver. Ved å bruke "-s" flagget, vil kommandoen også fjerne eventuelle duplikater av bokstaver.

## Dypdykk

For de som er interessert i litt mer teknisk informasjon, bruker "tr" kommandoen faktisk det som kalles for et "translation table". Dette er en tabell som inneholder to kolonner, en for de bokstavene du vil endre og en for hva du vil endre de til. Når kommandoen blir kjørt, sjekker den hver bokstav i stringen og ser om den finnes i den første kolonnen i tabellen. Hvis den finnes, vil den bli erstattet med bokstaven i den andre kolonnen.

Det finnes også andre måter å konvertere strings til lower case i Bash, som for eksempel ved hjelp av "sed" kommandoen eller ved å bruke "awk" kommandoen med "tolower" funksjonen. Det viktigste er å velge en metode som er mest effektiv og passer best til ditt spesifikke brukstilfelle.

## Se også

- [Bash Manual - tr command](https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html)
- [Bash Manual - sed command](https://www.gnu.org/software/sed/manual/sed.html)
- [Bash Manual - awk command](https://www.gnu.org/software/gawk/manual/gawk.html)