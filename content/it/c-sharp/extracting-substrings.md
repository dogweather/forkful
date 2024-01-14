---
title:    "C#: Estrazione di sottostringhe"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Perché

Estrarre sottostringhe può sembrare una semplice operazione, ma in realtà è un'importante abilità da avere quando si lavora con stringhe in C#. Ci permette di manipolare e ottenere parti specifiche di una stringa in modo efficiente e preciso.

## Come fare

Per estrarre una sottostringa in C#, possiamo utilizzare il metodo `Substring()` della classe `String`. Questo metodo richiede due parametri: l'indice di inizio e la lunghezza della sottostringa desiderata.

Ad esempio, se abbiamo una stringa `"Ciao amici!"` e vogliamo estrarre solo la parola `"amici"`, possiamo utilizzare il seguente codice:

```C#
string greeting = "Ciao amici!";
string friends = greeting.Substring(5,5);
Console.WriteLine(friends);
//Output: amici
```

In questo caso, abbiamo specificato che la nostra sottostringa inizia all'indice 5 (il primo carattere è considerato indice 0) e ha una lunghezza di 5 caratteri.

Possiamo anche utilizzare il metodo `Substring()` per estrarre sottostringhe da una posizione specifica fino alla fine della stringa, specificando semplicemente l'indice di inizio. Ad esempio:

```C#
string sentence = "Questa è una frase.";
string phrase = sentence.Substring(10);
Console.WriteLine(phrase);
//Output: una frase.
```

## Approfondimento

Il metodo `Substring()` è molto utile per vari scopi, come per esempio quando si vuole ottenere solo il nome di un file da un percorso completo o quando si lavora con stringhe di input da parte dell'utente.

Tuttavia, è importante ricordare che l'indice di inizio deve essere all'interno dei limiti della stringa originale per evitare errori. Inoltre, il metodo `Substring()` restituisce una nuova stringa invece di modificare quella originale, quindi è importante assegnare il risultato ad una nuova variabile o utilizzarlo in un'operazione successiva.

## Vedi anche

- Metodo `Substring()` nella documentazione di Microsoft: https://docs.microsoft.com/en-us/dotnet/api/system.string.substring
- Tutorial su stringhe in C#: https://www.programmareincsharp.it/stringhe-csharp/
- Esempi di utilizzo del metodo `Substring()`: https://www.engineersedge.com/programming/C_vs_vb_net_strings_15047.html