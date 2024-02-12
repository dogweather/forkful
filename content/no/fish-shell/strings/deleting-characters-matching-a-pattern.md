---
title:                "Slette tegn som matcher et mønster"
aliases:
- /no/fish-shell/deleting-characters-matching-a-pattern/
date:                  2024-01-20T17:42:13.744614-07:00
model:                 gpt-4-1106-preview
simple_title:         "Slette tegn som matcher et mønster"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å slette tegn som matcher et mønster betyr å fjerne bestemte sekvenser fra tekststrenger. Programmerere gjør dette for å rense data, ekstrahere informasjon eller formatere output.

## How to:
For å slette tegn i Fish Shell, bruk `string replace`. Eksempel fjerner alle 'a'-er fra en streng:

```Fish Shell
echo "bananarama" | string replace -a "a" ""
```

Output:
```
bnnrm
```

For å slette mønstre, bruk wildcards (stjernetegn):

```Fish Shell
echo "b123a456xyz789" | string replace -ar "[0-9]" ""
```

Output:
```
baxyz
```

Interaktivt, slett 'xyz' fra en variabel:

```Fish Shell
set myvar "helloxyzworld"
string replace -r "xyz" "" -- $myvar
echo $myvar
```

Output:
```
helloworld
```

## Deep Dive
Pattern matching har vært en del av programmering siden tidlige dager. Det er andre måter å gjøre det på i Unix-lignende systemer, som `sed` eller `grep`. Men Fish Shell forenkler prosessen med `string`-kommandoer.

Fish's `string` kommandoer ble introdusert for lesbarhet og enkel bruk. Ulik tradisjonell 'sed', trenger du ikke å huske komplekse syntakser for vanlige oppgaver.

Implementasjonsdetaljer:

- `string replace` er bygd inn i Fish.
- `-a` flagget står for "all" - erstatt alle instanser.
- `-r` aktiverer regex eller regulære uttrykk for avanserte mønstre.

## See Also
- [Fish Shell Documentation on String](https://fishshell.com/docs/current/cmds/string.html)
- [Regular Expressions Basics](https://en.wikipedia.org/wiki/Regular_expression)
- [Unix sed command](https://www.gnu.org/software/sed/manual/sed.html)
