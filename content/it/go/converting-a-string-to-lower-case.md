---
title:    "Go: Conversione di una stringa in minuscolo"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Perché

Prima di parlare di come convertire una stringa in minuscolo utilizzando il linguaggio di programmazione Go, è importante comprendere il motivo per cui potresti doverlo fare. Spesso, quando si lavora con stringhe, è necessario confrontare e manipolare i dati in modo che siano uniformi. Ad esempio, potresti voler confrontare due stringhe per vedere se sono uguali, indipendentemente dal fatto che una sia scritta in lettere maiuscole o minuscole.

# Come fare

La conversione di una stringa in minuscolo è un'operazione semplice utilizzando Go. Innanzitutto, dobbiamo importare il pacchetto "strings", che contiene la funzione "ToLower". Ecco un esempio di codice che mostra come utilizzare questa funzione:

```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    testo := "CIAO AMICI"
    minuscolo := strings.ToLower(testo)
    fmt.Println(minuscolo)
}
```

L'output di questo codice sarà "ciao amici", poiché la funzione "ToLower" ha convertito la stringa in minuscolo. È importante notare che questo metodo di conversione non modifica la stringa originale, ma ne restituisce una nuova in minuscolo.

# Approfondimento

Esistono diverse considerazioni da tenere a mente quando si utilizza la funzione "ToLower" per convertire una stringa in Go:

- Unicode: Go gestisce correttamente la conversione dei caratteri unicode in minuscolo, quindi non dovrai preoccuparti di errori di conversione.
- ASCII: se la stringa contiene solo caratteri ASCII, la conversione in minuscolo è piuttosto semplice. Tuttavia, se la stringa contiene caratteri non ASCII come lettere accentate o caratteri speciali, la conversione potrebbe non funzionare come previsto. In questo caso, potresti dover utilizzare un pacchetto esterno come "unicode/utf8" per gestire la conversione correttamente.
- Efficienza: poiché la funzione "ToLower" restituisce una nuova stringa, potrebbe essere inefficiente se si lavora con stringhe molto grandi o in un loop che richiama la funzione molte volte. In questi casi, potrebbe essere più efficiente utilizzare il pacchetto "strings.Builder" o la funzione "bytes.ToLower" per convertire direttamente la stringa originale in minuscolo.

# Vedi anche

- Documentazione ufficiale su "strings.ToLower" in Go: https://golang.org/pkg/strings/#ToLower
- Pacchetto "unicode/utf8" per gestire caratteri non ASCII: https://golang.org/pkg/unicode/utf8/
- Pacchetto "strings.Builder" per costruire stringhe in modo efficiente: https://golang.org/pkg/strings/#Builder
- Funzione "bytes.ToLower" per convertire una slice di byte in minuscolo: https://golang.org/pkg/bytes/#ToLower