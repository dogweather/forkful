---
title:    "Go: Oppretting av en midlertidig fil"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/go/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Hvorfor lage midlertidige filer i Go

Det kan hende du noen ganger trenger å lagre midlertidig data i løpet av en Go-programmeringsoppgave. Dette kan være for å behandle store datasett, teste ut nye funksjoner eller for å holde styr på midlertidige verdier. Opprettelse av en midlertidig fil er en enkel og effektiv måte å håndtere dette på.

## Slik gjør du det i Go

```Go
tempFile, err := ioutil.TempFile("", "GOMarkdown")
if err != nil {
    log.Fatal("Kan ikke opprette midlertidig fil.")
}
defer tempFile.Close()

fmt.Println("Navnet på midlertidig fil er:", tempFile.Name())
```
Eksempelet over viser hvordan du kan opprette en midlertidig fil ved hjelp av Go's `ioutil` pakke. Først opprettes en fil med `ioutil.TempFile()` metoden, som tar inn to argumenter. Det første argumentet er et prefix for filnavnet, og det andre argumentet er prefikset for utvidelsen som skal brukes. Deretter må du sørge for å lukke filen når du er ferdig med å bruke den ved å bruke `defer`-nøkkelordet. Til slutt viser vi navnet på den midlertidige filen.

## Dykk dypere ned

For å forstå mer om oppretting av midlertidige filer i Go, kan det være nyttig å vite at filene som opprettes er i det midlertidige filsystemet til operativsystemet ditt. Hastigheten på opprettingen vil derfor avhenge av hvor raskt det midlertidige filsystemet er.

Oppretting av en midlertidig fil er også fleksibel, og hvis du vil at filen skal slettes når du er ferdig med å bruke den, kan du bruke `tempFile.Delete()` metoden. Du kan også endre plasseringen til den midlertidige filen ved å bytte ut det første argumentet i `ioutil.TempFile()` med ønsket plassering.

## Se også

- [Go's `ioutil` pakke dokumentasjon](https://golang.org/pkg/io/ioutil/)
- [Mer informasjon om midlertidige filer i Go](https://www.digitalocean.com/community/tutorials/how-to-use-ioutil-package-in-go)