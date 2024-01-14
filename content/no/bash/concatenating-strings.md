---
title:    "Bash: Sammenføyning av strenger"
keywords: ["Bash"]
---

{{< edit_this_page >}}

# Hvorfor

Å kombinere eller "concatenate" strenger er en viktig del av Bash-programmering. Det lar deg sette sammen to eller flere tekststrenger for å danne en lengre streng. Dette kan være nyttig for å lage dynamiske meldinger, bygge filnavn, og mye mer.

# Hvordan

Det er flere måter å kombinere strenger på i Bash. Den enkleste og mest intuitive måten er å bruke operatorer.

```Bash
streng1="Hei,"
streng2="verden!"
streng3="$streng1 $streng2"
echo $streng3 # Output: Hei, verden!
```

I dette eksemplet definerer vi to separate strenger, "Hei," og "verden!" og deretter kombinerer vi dem ved å bruke operatorer og lagrer det i en ny streng, "streng3". Når vi bruker "echo" kommandoen for å skrive ut "streng3" vil vi få "Hei, verden!" som output.

En annen måte å kombinere strenger på er å bruke "printf" kommandoen.

```Bash
navn="Karoline"
printf "Hei, %s!" $navn # Output: Hei, Karoline!
```

Her bruker vi "printf" kommandoen til å lage en dynamisk melding, i dette tilfellet en hilsen til Karoline. Vi bruker "%s" for å spesifisere hvor i meldingen variabelen $navn skal plasseres.

# Dypdykk

I Bash er det også mulig å kombinere strenger ved hjelp av "+= "operator. Dette lar deg legge til en ny streng i en allerede definert strengvariabel.

```Bash
melding="Jeg elsker"
melding+=" å kode i Bash!"
echo $melding # Output: Jeg elsker å kode i Bash!
```

En annen nyttig måte å kombinere strenger på er ved hjelp av "here string" konstruksjonen.

```Bash
lesennavn()
{
    read -d \n -r melding
}

lesennavn <<< "Navnet mitt er Karoline, og jeg elsker å kode i bash!"
echo $melding # Output: Navnet mitt er Karoline, og jeg elsker å kode i Bash!
```

Her bruker vi "read" kommandoen for å lese en "herestring" og lagre det i variabelen "melding". Slik kan vi ta i mot innspill fra en bruker og deretter kombinere den med andre strenger.

# Se også

- [Bash - kombinere strenger](https://www.tldp.org/LDP/abs/html/string-manipulation.html)
- [GNU Bash Manual - 3.5.7 Shell Parameter Expansion](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html)
- [Linuxize - Bash - herestring](https://linuxize.com/post/bash-heredoc/)