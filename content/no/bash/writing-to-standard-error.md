---
title:    "Bash: Skriver til standard error"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/bash/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hvorfor

Det å skrive til standard error i Bash programmering kan være svært nyttig for å feilsøke og fange feil i koden din. Det er en enkel måte å få mer informasjon om hva som går galt og hvor i koden det skjer.

## Hvordan du gjør det

Å skrive til standard error i Bash er enkelt. Du bruker bare ">" eller "2>" i kommandoen din, etterfulgt av filbanen til loggfilen hvor du vil skrive feilmeldinger og annen informasjon. La oss si at du vil skrive en feilmelding til en fil kalt "error_log" i din nåværende mappe. Dette er hvordan koden din vil se ut:

```Bash
ls -l error_log 2> error_log
```

Dette vil skrive eventuelle feilmeldinger eller informasjon om kommandoen din til filen "error_log". Du kan også bruke ">>" for å legge til informasjon i en eksisterende fil, i stedet for å overskrive den.

```Bash
mkdir new_folder 2>> error_log
```

I tillegg kan du også skrive til både standard output og standard error på samme tid ved å bruke "&>". For eksempel:

```Bash
ls -l &> output_and_error_log
```

Dette vil skrive både standard output og standard error til filen "output_and_error_log". Det kan være nyttig når du ønsker å samle all informasjon om en kommando på ett sted.

## Dypdykk

Når du skriver til standard error i Bash, bruker du egentlig bare det samme konseptet som når du skriver til standard output. Forskjellen er at standard error er der for å skrive ut feilmeldinger og informasjon om kommandoer som ikke lykkes, mens standard output er for å skrive ut normal informasjon. Det er derfor viktig å forstå forskjellen mellom disse to og vite når du skal bruke ">", "2>" eller "&>".

Det er også verdt å nevne at standard error og standard output ikke nødvendigvis vil skrive ut i den rekkefølgen som kommandoene blir kjørt. Dette kan avhenge av hvordan programmet ditt er satt opp og hva slags informasjon som blir produsert.

## Se også

- [Bash skripting for nybegynnere](https://www.ntnu.no/wiki/display/itinfo/Bash+skripting+for+nybegynnere)
- [Bash referanser](https://www.gnu.org/software/bash/manual/html_node/Redirections.html)
- [Feilsøking og fangst av Bash feil](https://www.digitalocean.com/community/tutorials/how-to-debug-bash-scripts)