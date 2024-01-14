---
title:    "C#: Maiuscolare una stringa"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Perché
Perché dovremmo preoccuparci di capitalizzare una stringa nel nostro codice C#? La risposta è semplice: utilizziamo spesso stringhe nei nostri programmi e c'è la possibilità che queste debbano essere visualizzate in modo leggibile per gli utenti. La capitalizzazione è un modo per rendere più ordinata ed esteticamente gradevole la visualizzazione delle stringhe.

## Come fare
Per capitalizzare una stringa in C#, abbiamo diverse opzioni disponibili. Possiamo utilizzare il metodo `ToUpper()` per convertire tutti i caratteri in maiuscolo o il metodo `ToLower()` per convertirli tutti in minuscolo.

```
string frase = "ciao a Tutti!";
Console.WriteLine(frase.ToUpper()); // Output: CIAO A TUTTI!
Console.WriteLine(frase.ToLower()); // Output: ciao a tutti!
```

Possiamo anche utilizzare il metodo `ToTitleCase()` per convertire la prima lettera di ogni parola in maiuscolo.

```
string titolo = "il gatto sorridente";
Console.WriteLine(titolo.ToTitleCase()); // Output: Il Gatto Sorridente
```

Inoltre, possiamo utilizzare il metodo `Replace()` per sostituire una determinata lettera o un gruppo di lettere all'interno della nostra stringa. Ad esempio, se vogliamo sostituire tutte le lettere "a" con "o" nella parola "casa", possiamo farlo in questo modo:

```
string parola = "casa";
Console.WriteLine(parola.Replace("a", "o")); // Output: coso
```

È importante notare che questi metodi non modificano la stringa originale, ma restituiscono una nuova stringa modificata. Quindi, se vogliamo applicare una di queste modifiche alla nostra stringa originale, dobbiamo assegnare nuovamente il valore alla variabile.

## Approfondimento
Ma quali sono i tipi di caratteri che possono essere capitalizzati? Possiamo capitalizzare solo lettere dell'alfabeto, o possiamo capitalizzare anche numeri e caratteri speciali? La risposta è che dipende dallo scopo della nostra applicazione.

Se vogliamo solo visualizzare una stringa in modo più ordinato, possiamo utilizzare i metodi descritti sopra e capitalizzare solo lettere dell'alfabeto. Tuttavia, se dobbiamo verificare l'uguaglianza tra stringhe e vogliamo che anche le differenze di capitalizzazione siano considerate, dovremmo utilizzare il metodo `Equals()` passando come parametro il valore di confronto e l'oggetto `StringComparison.OrdinalIgnoreCase`.

## Vedi anche
- Documentazione ufficiale su stringhe in C# (https://docs.microsoft.com/it-it/dotnet/csharp/programming-guide/strings/)
- Tutorial su come manipolare le stringhe in C# (https://www.tutorialspoint.com/csharp/csharp_strings.htm)
- Articolo su come gestire correttamente la capitalizzazione in C# (https://www.c-sharpcorner.com/blogs/understanding-string-comparison-in-C-Sharp1)