---
title:                "Java: Convertire una stringa in minuscolo"
simple_title:         "Convertire una stringa in minuscolo"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Perché

La conversione di una stringa in caratteri minuscoli può essere utile in diverse situazioni, come ad esempio quando si vuole confrontare due stringhe senza distinzioni tra maiuscole e minuscole o quando si vuole ottenere un output uniforme in un determinato formato.

## Come fare

La conversione di una stringa in caratteri minuscoli in Java è molto semplice. Basta utilizzare il metodo `toLowerCase()` sulle variabili di tipo stringa. Ecco un esempio di codice:

```Java
String nome = "MARIO";
String cognome = "ROSSI";
String nomeCompleto = nome.toLowerCase() + " " + cognome.toLowerCase();

System.out.println("Nome completo: " + nomeCompleto);
```

Ecco l'output di questo codice:

```
Nome completo: mario rossi
```

## Approfondimento

Java utilizza il "Unicode standard" per rappresentare i caratteri e questo può influire sulla conversione in caratteri minuscoli. Ad esempio, il carattere "ß" viene convertito in "ss" e non in "ß". Inoltre, il metodo `toLowerCase()` può essere sovrascritto dalle classi figlie di `String`, quindi il comportamento della conversione potrebbe variare in base alla classe specifica utilizzata.

## Vedi anche

- [Documentazione ufficiale del metodo `toLowerCase()` in Java](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#toLowerCase--)
- [Altro metodo utile per la manipolazione di stringhe in Java](https://www.java.com/it/download/help/string_api.html)