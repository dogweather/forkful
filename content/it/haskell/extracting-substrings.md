---
title:    "Haskell: Ottenere sottostringhe."
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Perché
L'estrazione di sottostringhe è un'importante abilità nel linguaggio di programmazione Haskell che permette di lavorare con stringhe in modo più flessibile e preciso. Con l'estrazione di sottostringhe, è possibile ottenere una parte specifica di una stringa, come ad esempio una parola o una frase, e utilizzarla per compiere altre operazioni. Questa capacità può risultare molto utile in diverse situazioni e quindi vale la pena imparare come implementarla correttamente.

## Come fare
L'estrazione di sottostringhe in Haskell può essere realizzata utilizzando principalmente due funzioni: `take` e `drop`. La funzione `take` prende come argomenti una lunghezza e una lista e restituisce i primi elementi della lista pari alla lunghezza specificata. Ad esempio, se si vuole estrarre i primi 3 caratteri di una stringa, si può utilizzare `take 3 "Haskell"`, ottenendo così "Has". La funzione `drop`, invece, prende come argomenti una lunghezza e una lista e restituisce la lista senza i primi elementi della lunghezza specificata. Utilizzando gli stessi esempi di prima, `drop 3 "Haskell"` restituirà "kell".

Un altro metodo per estrarre una sottostringa è utilizzare la funzione `substring` del modulo `Data.List`. Questa funzione richiede come argomenti una posizione di inizio e una posizione di fine e restituisce la sottostringa compresa tra tali posizioni. Ad esempio, `substring 2 4 "Haskell"` restituirà "sk".

Ecco un esempio pratico di come estrarre sottostringhe in Haskell:

```Haskell
import Data.List  

sentence = "Il gatto è sul tappeto."

-- Utilizzando take e drop
firstWord = take 2 sentence -- restituisce "Il"
lastWord = drop 18 sentence -- restituisce "tappeto."

-- Utilizzando substring
middleWord = substring 9 11 sentence -- restituisce "è"
```

## Approfondimento
È possibile utilizzare altri metodi e funzioni per estrarre sottostringhe in Haskell. Ad esempio, la funzione `splitAt` del modulo `Data.List` permette di dividere una stringa in due parti separate in una posizione specifica. Inoltre, il modulo `Data.Text` offre opzioni più efficienti per la gestione di stringhe lunghe.

È importante tenere presente che l'estrazione di sottostringhe può essere un'operazione costosa, soprattutto su stringhe molto lunghe, poiché richiede la creazione di una nuova stringa. Pertanto, è consigliabile utilizzare layz evaluation quando possibile per evitare di elaborare stringhe inutili.

## Vedi anche
- [Funzione `take` del modulo `Prelude`](https://hackage.haskell.org/package/base-4.14.0.0/docs/Prelude.html#v:take)
- [Funzione `drop` del modulo `Prelude`](https://hackage.haskell.org/package/base-4.14.0.0/docs/Prelude.html#v:drop)
- [Funzione `substring` del modulo `Data.List`](https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-List.html#v:substring)