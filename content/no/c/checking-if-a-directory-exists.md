---
title:    "C: Å sjekke om en mappe eksisterer"
keywords: ["C"]
---

{{< edit_this_page >}}

# Hvorfor

Det er ofte lurt å sjekke om en mappe eksisterer før du prøver å gjøre noe med den i et C-program. Dette kan unngå uønskede feil og sikre en bedre brukeropplevelse.

# Hvordan

For å sjekke om en mappe eksisterer, kan du bruke "opendir" funksjonen i C-programmering. Dette vil åpne en filpeker til den gitte mappen, og hvis den ikke eksisterer, vil den returnere en nullpeker. Her er et eksempel på hvordan dette kan gjøres:

```C
DIR *mappe = opendir("/sti/til/mappe");

if (mappe == NULL) {
    printf("Mappen eksisterer ikke.\n");
} else {
    printf("Mappen eksisterer.\n");
    closedir(mappe); // husk å lukke filpekeren når du er ferdig!
}
```

Eksemplet ovenfor sjekker om mappen "/sti/til/mappe" eksisterer og skriver ut en passende melding basert på resultatet.

# Deep Dive

For de som ønsker en dypere forståelse av "opendir" funksjonen, kan vi se på beskrivelsen i C standardbiblioteket. Funksjonen er definert som:

```C
DIR *opendir(const char *path);
```

Det første argumentet er en streng som inneholder stien til mappen du ønsker å åpne. Hvis mappen ikke eksisterer, vil "opendir" returnere en nullpeker.

Det andre argumentet, "DIR *", er en filpeker til mappen. Dette kan brukes til å utforske innholdet i mappen, som for eksempel å liste ut filnavnene.

Det er også verdt å nevne at "opendir" funksjonen ikke bare kan brukes til å sjekke om en mappe eksisterer. Den kan også brukes til å åpne en mappe for å lese eller skrive filer.

# Se Også

- [dokumentasjon for "opendir" i C standardbiblioteket](https://www.gnu.org/software/libc/manual/html_node/Opening-a-Directory.html#Opening-a-Directory)
- [en artikkel om "opendir" og relasjonen til "readdir" og "closedir"](https://www.geeksforgeeks.org/different-ways-opening-file-c/)