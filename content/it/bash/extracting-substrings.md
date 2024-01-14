---
title:                "Bash: Estrazione di sottostringhe"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

## Perché
Se sei un programmatore, potresti aver bisogno di estrarre una parte di testo da una stringa più grande. Ad esempio, potresti voler recuperare solo il nome del file da un percorso completo o estrarre una data specifica da una stringa di testo. In entrambi i casi, l'estrazione di sottostringhe può essere molto utile.

## Come fare
Per estrarre una sottostringa in Bash, usa il comando "cut". Per esempio, se voglio estrarre i primi 5 caratteri da una stringa, posso usare il seguente codice:
```Bash
stringa="Questo è un esempio"
echo "${stringa:0:5}"
```
Questo produrrà l'output "Quest".

Per estrarre una sottostringa in base a un carattere specifico, puoi usare il comando "awk". Ad esempio, per estrarre la parte della stringa dopo la terza occorrenza di un carattere, puoi usare questo codice:
```Bash
stringa="Questo è un esempio"
echo "${stringa#*[*]}"
```
Questo produrrà l'output "un esempio". Il simbolo "#" indica che vuoi rimuovere tutto ciò che si trova prima della terza occorrenza del carattere specificato (in questo caso, "*").

## Approfondimento
Bash offre diverse opzioni per l'estrazione di sottostringhe, tra cui l'uso di espressioni regolari e il comando "sed". Inoltre, puoi anche utilizzare la funzione "expr" per estrarre sottostringhe in base alla loro posizione.

Tuttavia, è importante notare che l'estrazione di sottostringhe non è sempre la soluzione migliore per risolvere un problema. In alcuni casi, potrebbe essere più efficiente utilizzare altri comandi o strumenti. Pertanto, è importante valutare attentamente se l'utilizzo di questo approccio è la scelta migliore per il tuo scopo specifico.

## Vedi anche
- [La guida ufficiale di Bash](https://www.gnu.org/software/bash/manual/bash.html)
- [Esempi di espressioni regolari con Bash](https://tecadmin.net/bash-regular-expression-tutorial/)