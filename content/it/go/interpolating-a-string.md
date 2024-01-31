---
title:                "Interpolazione di una stringa"
date:                  2024-01-20T17:51:02.689408-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolazione di una stringa"

category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Cosa e Perché?)
L'interpolazione di stringhe è il processo di inserimento di valori dinamici in una stringa fissa. I programmatori lo fanno per creare messaggi, dati e output che sono personalizzati ed esatti senza scrivere stringhe differenti per ogni situazione.

## How to: (Come fare:)
```Go
package main

import (
	"fmt"
)

func main() {
	name := "Francesco"
	age := 35

	// Interpolazione con Sprintf
	message := fmt.Sprintf("Ciao, mi chiamo %s e ho %d anni.", name, age)
	fmt.Println(message)

	// Interpolazione con template string (recentemente aggiunto in Go)
	greeting := `Ciao, mi chiamo {{.Name}} e ho {{.Age}} anni.`
	tmpl, err := template.New("test").Parse(greeting)
	if err != nil {
		panic(err)
	}
	data := struct {
		Name string
		Age  int
	}{
		Name: name,
		Age:  age,
	}
	err = tmpl.Execute(os.Stdout, data)
	if err != nil {
		panic(err)
	}
}
```

Output:
```
Ciao, mi chiamo Francesco e ho 35 anni.
Ciao, mi chiamo Francesco e ho 35 anni.
```

## Deep Dive (Approfondimento)
Il concetto di interpolazione di stringhe esiste da molto tempo nei linguaggi di programmazione. In Go, per anni la strada standard è stata quella di usare `fmt.Sprintf`, che permette di inserire variabili all'interno di una stringa con un certo formato. Con l'introduzione del pacchetto `text/template`, Go offrì un altro modo di interpolare stringhe, adatto soprattutto quando si hanno template di testo complessi. L'interpolazione avviene attraverso i "placeholders" (segnaposto) nei template che vengono poi sostituiti con dati concreti. Nel caso di `Sprintf`, usiamo dei verbi di formato, come `%s` per le stringhe e `%d` per i numeri interi. Con i template, invece, utilizziamo una notazione a doppia parentesi graffa `{{}}`.

## See Also (Vedi Anche)
- Per approfondire il pacchetto `fmt`, controlla la documentazione ufficiale: https://golang.org/pkg/fmt/
- Per esplorare in dettaglio il pacchetto `text/template`, visita: https://golang.org/pkg/text/template/
- Un buon tutorial su come lavorare con i template di testo in Go: https://blog.gopheracademy.com/advent-2017/using-go-templates/
