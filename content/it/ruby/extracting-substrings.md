---
title:    "Ruby: Estrazione di sottostringhe"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Perché
L'estrazione di sottotringhe è una pratica comune nella programmazione Ruby che consente di ottenere solo una parte di una stringa più grande. Ciò può essere utile per estrarre informazioni specifiche o per manipolare i dati in modo più efficiente.

## Come Estrarre Sottotringhe in Ruby
Per estrarre sottotringhe in Ruby, è possibile utilizzare il metodo `[]` che accetta uno o due argomenti. L'argomento facoltativo può essere un indice o un intervallo che specifica la posizione all'interno della stringa da cui estrarre i caratteri desiderati.

```Ruby
# Esempio di estrazione di un singolo carattere
stringa = "Ciao Mondo"
puts stringa[0] # Output: C

# Esempio di estrazione di un intervallo
puts stringa[0..3] # Output: Ciao
```

## Approfondimento sull'Estrazione di Sottotringhe
Per estrarre sottotringhe in modo più avanzato, è possibile combinare il metodo `[]` con altri metodi stringa e array come `split` e `slice`. Inoltre, è possibile utilizzare espressioni regolari per trovare pattern specifici all'interno di una stringa.

Un altro argomento importante da considerare è la gestione degli indici negativi, che iniziano a contare dalla fine della stringa. Ad esempio, `-1` rappresenta l'ultimo carattere, `-2` il penultimo e così via.

## Vedere Anche
- [Documentazione ufficiale di Ruby sull'estrazione di sottotringhe](https://ruby-doc.org/core-2.7.1/String.html#method-i-5B-5D)
- [Estrazione di sottotringhe in Ruby: tutorial completo](https://www.rubyguides.com/2018/10/ruby-string-methods/#12stringmethod7)
- [Utilizzo di espressioni regolari per estrarre sottotringhe in Ruby](https://www.regular-expressions.info/ruby.html)