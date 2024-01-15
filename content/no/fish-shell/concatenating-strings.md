---
title:                "Sammenslåing av strenger."
html_title:           "Fish Shell: Sammenslåing av strenger."
simple_title:         "Sammenslåing av strenger."
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hvorfor

Å kombinere strenger kan være en nyttig teknikk når man arbeider med tekstdokumenter og dataprogrammering generelt. Ved å kombinere flere strenger kan man lage lengre og mer varierte tekster og variabler, og dermed oppnå mer kompleks funksjonalitet i programmet ditt.

## Hvordan

For å kombinere strenger i Fish Shell, bruker man operatoren `+` eller `string join`. Her er et eksempel som kombinerer to strenger og lagrer resultatet i en variabel kalt `concatenated_string`:

```Fish Shell
set concatenated_string "Hei " + "verden!"
echo $concatenated_string

# Output: Hei verden!
```

Man kan også kombinere flere strenger ved å bruke `string join` og angi et mellomromskarakter for å skille dem:

```Fish Shell
set names "Bob" "Jill" "Alice"
set combined_names (string join " " $names)
echo "Navnene er: " $combined_names

# Output: Navnene er: Bob Jill Alice
```

## Dypdykk

Fish Shell har også flere muligheter for å formatere og kombinere strenger på avansert nivå. Ved hjelp av `string sub` kan man for eksempel bytte ut deler av en streng med en annen streng:

```Fish Shell
set name "Peter"
set formatted_name (string sub -r -a "P" "B" $name)
echo "Det riktige navnet er: " $formatted_name

# Output: Det riktige navnet er: Better
```

Man kan også bruke `string replace` for å bytte ut deler av en streng med en annen:

```Fish Shell
set sentence "Jeg liker katter"
set new_sentence (string replace "katter" "hunder" $sentence)
echo "Men jeg liker faktisk hunder mer, så jeg vil si: " $new_sentence

# Output: Men jeg liker faktisk hunder mer, så jeg vil si: Jeg liker hunder
```

## Se også

- Offisiell dokumentasjon for Fish Shell: https://fishshell.com/docs/current/index.html
- En enkel guide til å komme i gang med Fish Shell: https://github.com/jorgebucaran/fish-shell-cookbook/blob/master/README.md#the-one-minute-introduction