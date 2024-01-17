---
title:                "Skriver til standardfeil"
html_title:           "Bash: Skriver til standardfeil"
simple_title:         "Skriver til standardfeil"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Når du programmerer i Bash, kan du skrive ut informasjon til standard error i stedet for standard output. Dette er nyttig for å skrive ut feilmeldinger og annen viktig informasjon som ikke skal blandes med vanlig utdata.

## Slik gjør du:
For å skrive til standard error, bruker du kommandoen `>&2`. La oss si at du har en variabel som inneholder en feilmelding, kalt `error_msg`, og du vil skrive den ut til standard error. Da skriver du `echo $error_msg >&2`. Dette vil skrive ut `error_msg` til standard error i stedet for standard output.

```
Bash eksempel:
error_msg="Noe gikk galt!"
echo $error_msg >&2

Output:
Noe gikk galt!
```

## Dypdykk:
Siden Bash er en skall, ble standard error først introdusert i Bourne Shell i 1979. Før dette var det vanlig praksis å sende feilmeldinger til standard output, men dette kunne være forvirrende og vanskelig å få tak i når man kjørte større skall-script. Alternativet til å skrive til standard error er å sende utdata til en loggfil for senere referanse. Dette er spesielt nyttig i produksjonsmiljøer.

## Se også:
[The Linux Command Line: Writing to Standard Error](http://linuxcommand.org/lc3_man_pages/wget1.html) <br>
[Bash programming tutorial](https://ryanstutorials.net/bash-scripting-tutorial/)