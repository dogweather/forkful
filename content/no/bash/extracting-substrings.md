---
title:                "Uttrekk av understrenger"
html_title:           "Bash: Uttrekk av understrenger"
simple_title:         "Uttrekk av understrenger"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor vil du ekstrahere undertrenger? Vel, ofte i Bash-programmering, vil du måtte behandle data eller tekster som instruksjoner på bestemte måter. Ekstrahering av undertrenger er en av disse teknikkene som kan få jobben gjort.

## Hvordan

Først må du ha en variabel som inneholder teksten du vil ekstrahere undertrenger fra.

```Bash
tekst="Dette er en tekst som inneholder undertrenger."
```

Deretter kan du bruke `${variabel: startposisjon: lengde}` - syntaksen for å ekstrahere undertrenger.

```Bash
echo ${tekst:11:6}
# Output: tekst
```

Her angir `startposisjon` hvor i teksten du vil begynne å ekstrahere, og `lengde` er hvor mange tegn du vil ekstrahere. I eksempelet ovenfor starter vi på posisjon 11 (teller starter fra 0) og ekstraherer 6 tegn, som gir oss undertrengeren "tekst".

Hvis du vil ekstrahere undertrengere fra starten av teksten, kan du bare utelate `startposisjon`.

```Bash
echo ${tekst::4}
# Output: Dett
```

Du kan også bruke negative tall for `startposisjon` eller `lengde`, som vil telle fra slutten av teksten.

```Bash
echo ${tekst: -9: 8}
# Output: ondertr
```

## Dypdykk

Nå som du har en forståelse av hvordan du ekstraherer undertrenger, kan du også bruke wildcards i syntaksen.

```Bash
echo ${tekst:6: *e*}
# Output: er
```

Dette vil ekstrahere en undertrenger som inneholder bokstaven "e" fra posisjon 6.

En annen nyttig teknikk er å bruke `#` og `%` for å ekstrahere undertrenger basert på mønstermatching.

```Bash
echo ${tekst#*tekst}
# Output: som inneholder undertrenger.
echo ${tekst%.*}
# Output: Dette er en tekst som inneholder undertrenger
```

## Se Også

- [Bash dokumentasjon](https://www.gnu.org/software/bash/manual/bash.html)
- [Bash scripting guide](https://ryanstutorials.net/bash-scripting-tutorial/)