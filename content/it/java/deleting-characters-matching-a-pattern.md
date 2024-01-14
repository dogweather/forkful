---
title:                "Java: Eliminazione di caratteri corrispondenti ad un modello."
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molte situazioni in cui è necessario rimuovere caratteri che corrispondono ad un determinato pattern da una stringa. Ad esempio, potresti voler eliminare tutti i numeri da un input utente o rimuovere determinati caratteri da un file di testo. In questi casi, è utile sapere come eliminare facilmente i caratteri corrispondenti al pattern desiderato da una stringa. In questo post, spiegheremo come farlo utilizzando Java.

## Come fare

Per eliminare i caratteri che corrispondono ad un determinato pattern da una stringa in Java, possiamo utilizzare il metodo `replaceAll()` della classe `String`. Questo metodo accetta due parametri: il primo è il pattern che vogliamo sostituire e il secondo è la stringa di sostituzione, che può essere vuota se vogliamo eliminare semplicemente il pattern.

```Java
// Esempio di eliminazione di tutti i numeri da una stringa
String input = "Questa è una stringa con 123 numeri.";
String output = input.replaceAll("[0-9]", "");
System.out.println(output); // Output: "Questa è una stringa con numeri."

// Esempio di eliminazione di vocali da una stringa
String input = "Questo è un'altra stringa con molte vocali.";
String output = input.replaceAll("[aeiou]", "");
System.out.println(output); // Output: "Qst è '-ltrl strng cn mlt vlcl."
```

## Approfondimento

Il metodo `replaceAll()` utilizza espressioni regolari per cercare il pattern specificato all'interno della stringa e sostituirlo. Le espressioni regolari sono una potente e flessibile notazione per descrivere stringhe di testo. Ad esempio, `[0-9]` corrisponde a qualsiasi numero da 0 a 9, mentre `[aeiou]` corrisponde a qualsiasi vocale minuscola.

Se si desidera eliminare più di un pattern dalla stringa, è possibile utilizzare l'operatore logico OR (`|`) all'interno delle parentesi quadre. Ad esempio, `"[aeiou]|[0-9]"` eliminerà sia le vocali che i numeri dalla stringa.

Una cosa importante da notare quando si utilizzano espressioni regolari è che vengono interpretate come sequenze di caratteri comuni a meno che non sia specificato diversamente. Ad esempio, se si desidera eliminare il carattere punto (`.`) dalla stringa, è necessario utilizzare l'operatore di escape (`\`), altrimenti verrà interpretato come un carattere "qualunque".

## Vedi anche

- Tutorial Java per principianti: [Link](https://www.javatpoint.com/java-tutorial)
- Guida alle espressioni regolari in Java: [Link](https://www.zentut.com/java/regex-pattern/)
- Documentazione ufficiale di Java sul metodo `replaceAll()`: [Link](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#replaceAll-java.lang.String-java.lang.String-)