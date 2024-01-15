---
title:                "Maiuscolizzare una stringa"
html_title:           "Go: Maiuscolizzare una stringa"
simple_title:         "Maiuscolizzare una stringa"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Perché

Se stai cercando un modo semplice ed efficiente per capitalizzare una stringa nel tuo codice Go, sei nel posto giusto! In questo articolo scoprirai come farlo in pochi passaggi.

## Come fare

Per capitalizzare una stringa in Go, è possibile utilizzare la funzione `ToUpper` del pacchetto `strings`. Vediamo un esempio pratico:

```Go
import "strings"

func main() {
    stringa := "ciao a tutti!"
    stringaCapitalizzata := strings.ToUpper(stringa)
    fmt.Println(stringaCapitalizzata)
}
```

**Output:**

```
CIAO A TUTTI!
```

Come puoi vedere, abbiamo semplicemente utilizzato la funzione `ToUpper` passando come argomento la stringa da capitalizzare. Questa funzione restituisce una nuova stringa con tutti i caratteri convertiti in lettere maiuscole. 

Se vuoi capitalizzare solo la prima lettera di una stringa, puoi utilizzare la funzione `Title`:

```Go
func main() {
    stringa := "ciao a tutti!"
    stringaCapitalizzata := strings.Title(stringa)
    fmt.Println(stringaCapitalizzata)
}
```

**Output:**

```
Ciao A Tutti!
```

## Approfondimento

Adesso che sai come capitalizzare una stringa in Go, vediamo quali sono le tecniche utilizzate dalla funzione `ToUpper` per portare a termine questo compito.

In realtà, la funzione `ToUpper` fa uso della funzione `Map` del pacchetto `strconv` per cambiare il valore del byte rappresentante il carattere in lettera maiuscola. Questo processo viene ripetuto per ogni carattere della stringa.

È anche possibile creare una propria funzione per capitalizzare una stringa, utilizzando ad esempio l'operatore `+=` per concatenare i caratteri e la funzione `Upper` del pacchetto `unicode` per convertire i caratteri in lettere maiuscole. 

## Vedi anche

- [Go Strings Package Documentation](https://pkg.go.dev/strings)
- [Go Strconv Package Documentation](https://pkg.go.dev/strconv)
- [Go Unicode Package Documentation](https://pkg.go.dev/unicode)