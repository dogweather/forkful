---
title:    "Go: Ottenere la data corrente"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Perché

Molti progetti di programmazione richiedono di ottenere la data corrente come parte del loro funzionamento. Ad esempio, un'applicazione di prenotazione potrebbe voler mostrare la data attuale come predefinita nel modulo di prenotazione. La funzionalità di ottenere la data corrente è quindi importante per garantire un flusso di lavoro fluido e una migliore esperienza utente.

## Come Fare

In Go, ottenere la data corrente è semplice grazie alla libreria standard `time`. Per prima cosa, dobbiamo importare la libreria nel nostro programma con `import "time"`. Poi, possiamo utilizzare la funzione `Now()` per ottenere un `Time` che rappresenta la data e l'ora attuali.

```
import "time"

func main() {
    now := time.Now()
    fmt.Println(now)
}
```

L'output sarà nel formato `yyyy-mm-dd hh:mm:ss +0000 UTC`, in cui `yyyy` rappresenta l'anno, `mm` il mese, `dd` il giorno, `hh` l'ora, `mm` i minuti, `ss` i secondi e `UTC` il fuso orario. Possiamo anche formattare l'output con il metodo `Format()`.

```
func main() {
    now := time.Now()
    formattedTime := now.Format("02/01/2006 15:04:05")
    fmt.Println(formattedTime)
}
```

In questo caso, l'output sarà nel formato `dd/mm/yyyy hh:mm:ss`.

## Approfondimento

La funzione `Now()` di `time` utilizza il fuso orario `UTC` come riferimento. Possiamo anche ottenere la data e l'ora in un fuso orario specifico utilizzando la funzione `Location()` che prende come argomento il fuso orario desiderato. Inoltre, possiamo anche utilizzare il metodo `Add()` per aggiungere una durata di tempo a un `Time` specifico.

## Vedi Anche

- [Documentazione ufficiale di `time` in Go](https://golang.org/pkg/time/)
- [Come ottenere la data corrente in altri linguaggi di programmazione](https://www.freecodecamp.org/news/how-to-get-the-current-date-and-time-in-java-python-c-and-javascript/)