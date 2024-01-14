---
title:                "Go: Å få nåværende dato"
programming_language: "Go"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hvorfor

Å få tak i den gjeldende datoen er et vanlig behov for programmører uansett språk eller prosjekt. I denne bloggposten vil vi se nærmere på hvordan man kan få tak i datoen ved hjelp av Go-programmeringsspråket.

## Slik Gjør Du

For å få tak i den gjeldende datoen i Go, bruker vi funksjonen "Now" fra tidsmodulen. Her er et enkelt eksempel:

```Go
func main() {
   date := time.Now()
   fmt.Println(date)
}
```

Kjører vi dette eksempelet, vil vi få følgende utskrift:

```
2021-07-10 12:00:00
```

I dette eksempelet brukte vi "fmt" pakken til å skrive ut datoen. Vi kan imidlertid også formatere datoen på forskjellige måter ved å bruke "Format" funksjonen, som vist i følgende eksempel:

```Go
func main() {
   date := time.Now()
   fmt.Println(date.Format("02/01/2006"))
}
```

Denne gangen vil utskriften være:

```
10/07/2021
```

Her brukte vi formatet "02/01/2006" som tilsvarer "dag/måned/år". Ved å endre på rekkefølgen av tallene, kan vi formatere datoen på ulike måter. Dette er bare noen få eksempler på hvordan man kan få tak i datoen ved hjelp av Go-programmeringsspråket.

## Dypdykk

For å få en dypere forståelse av hvordan datoen blir hentet i Go, kan vi se nærmere på koden til "Now" funksjonen i tidsmodulen. Denne funksjonen bruker faktisk "Unixtimen", som er antallet sekunder som har gått siden 1. januar 1970. Go konverterer deretter dette til en lesbar dato og tid ved hjelp av ulike matematiske operasjoner.

Hvis du vil lære mer om hvordan datoen blir hentet i Go, kan du se på koden til "Now" funksjonen på offisielle dokumentasjonssiden til Go.

## Se Også

- Offisiell dokumentasjon for "Now" funksjonen i tidsmodulen: <https://golang.org/pkg/time/#Now>
- En liste over lovlige formater til "Format" funksjonen: <https://golang.org/src/time/format.go>
- En veiledning til Go programmeringsspråket: <https://golang.org/doc/>