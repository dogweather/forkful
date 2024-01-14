---
title:                "Java: Estrazione di sottostringhe"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/extracting-substrings.md"
---

{{< edit_this_page >}}

# Perché
Estrazione di sottostringhe è un'operazione comune e utile nella programmazione Java. Ci sono molte situazioni in cui è necessario estrarre una parte di testo da una stringa più grande. Ad esempio, potresti voler ottenere il nome di un file da un percorso completo del file o estrarre un numero di telefono da una stringa di testo.

# Come Fare
Per estrarre una sottostringa da una stringa in Java, puoi utilizzare il metodo `substring()` della classe `String`. Questo metodo richiede due argomenti: l'indice di inizio e l'indice di fine della sottostringa desiderata.

Ecco un esempio di codice che estrae una sottostringa dal nome completo di una persona e restituisce solo il cognome:

```Java
String nomeCompleto = "Mario Rossi";
String cognome = nomeCompleto.substring(6);
System.out.println(cognome); // output: Rossi
```

Nell'esempio sopra, l'indice di inizio è 6 perché il primo carattere della stringa è un indice 0. Se vogliamo includere anche il nome, possiamo specificare l'indice di fine come 5, come mostrato di seguito:

```Java
String cognome = nomeCompleto.substring(0, 5); // output: Mario
```

# Approfondimento
Oltre al metodo `substring()`, ci sono altri modi per estrarre sottostringhe in Java. Ad esempio, puoi utilizzare il metodo `split()` della classe `String` per dividere una stringa in base a un carattere o una espressione regolare specificata.

Inoltre, ci sono diverse classi nella libreria standard di Java che possono aiutarti a gestire le sottostringhe in modi più complessi, come ad esempio la classe `StringBuffer` o la classe `StringBuilder`.

# Vedi Anche
Se sei interessato a saperne di più sull'estrazione di sottostringhe in Java, puoi consultare questi utili link:

- [Documentazione ufficiale di Java sul metodo `substring()`](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/String.html#substring(int,int))
- [Tutorial di W3Schools su come estrarre sottostringhe in Java](https://www.w3schools.com/java/java_strings_substrings.asp)
- [Articolo su Baeldung sull'uso di classi come `StringBuffer` e `StringBuilder` per estrarre sottostringhe in Java](https://www.baeldung.com/java-substring)