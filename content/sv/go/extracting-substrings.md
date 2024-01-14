---
title:                "Go: Extrahera delsträngar"
programming_language: "Go"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/extracting-substrings.md"
---

{{< edit_this_page >}}

## Varför

I Go-programmering, eller något annat programmeringsspråk för den delen, finns det ofta tillfällen när vi behöver arbeta med strängar - en sekvens av tecken. Ibland kanske vi bara vill använda en del av en sträng, eller en "substring". I denna bloggpost kommer vi att titta på hur man extraherar substrängar i Go och varför det kan vara användbart.

## Hur man gör

För att extrahera en substräng i Go använder vi funktionen `strings.Replace`. Den tar fyra argument: den ursprungliga strängen, den del av strängen vi vill byta ut, den del av strängen vi vill ersätta den första delen med, och slutligen det valfria argumentet `n` som anger hur många gånger vi vill byta ut delen.

Låt oss säga att vi har en sträng "Hej världen" och vi bara vill ha "världen". Detta är hur vi skulle göra det i Go:

```Go
str := "Hej världen"
substr := strings.Replace(str, "Hej ", "", 1)

fmt.Println(substr) // världen
```

Vi anger att vi vill ersätta "Hej " med en tom sträng `""` och `1` indikerar att vi bara vill byta ut den första förekomsten av "Hej ".

Om vi istället vill extrahera en del av en sträng baserat på positioner använder vi funktionen `strings.Substring`. Den tar två argument: den ursprungliga strängen och de två positionerna för delen vi vill extrahera.

Låt oss säga att vi vill få ut "värld" från vår tidigare sträng "Hej världen". Så här skulle vi kunna göra det:

```Go
str := "Hej världen"
substr := str[4:9]

fmt.Println(substr) // värld
```

Här använde vi "slice notation" för att ange de två positionerna (4 och 9) som motsvarar början och slutet av den del av strängen vi vill ha.

## Djupdykning

Som vi nämnde tidigare är `strings.Substring` den funktion som används för att extrahera delar av strängar baserat på positioner. Men vad händer bakom kulisserna?

I själva verket är `strings.Substring` bara en bekvämlighetsfunktion som använder "slice notation" som vi tidigare använde. Om vi tittar på dess implementation i Go-källkoden så ser vi följande:

```Go
func Substring(s string, i, j int) string {
	if i < 0 || i > len(s) {
		panic("substring index out of bounds")
	}
	if j < 0 || j > len(s) {
		panic("substring index out of bounds")
	}
	if i > j {
		panic("substring index out of bounds")
	}
	return s[i:j]
}
```

Som vi kan se kontrollerar den bara att de angivna positionerna finns inom strängens längd och returnerar därefter en del av strängen. Så nu vet vi att när vi använder `strings.Substring` händer egentligen inget speciellt, vi kan lika gärna använda "slice notation" direkt.

## Se även

- [Go's officiella dokumentation om strängar](https://golang.org/pkg/strings/)
- [En djupare förklaring av "slice notation"](https://gobyexample.com/slices)
- [Fler användbara strängfunktioner i Go](https://www.golangprograms.com/golang-package-examples.html)