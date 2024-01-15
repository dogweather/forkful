---
title:                "Concatenare stringhe"
html_title:           "PHP: Concatenare stringhe"
simple_title:         "Concatenare stringhe"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/concatenating-strings.md"
---

{{< edit_this_page >}}

## Perché

Concatenare le stringhe è un'operazione fondamentale nella programmazione PHP. Consiste nell'unire due o più sequenze di caratteri per creare una nuova stringa. Ciò può essere utile in molte situazioni, ad esempio per costruire messaggi personalizzati o per formattare correttamente i dati prima di salvarli o stamparli.

## Come Fare

```PHP
$stringa1 = "Ciao ";
$stringa2 = "amici!";
$stringa3 = $stringa1 . $stringa2;
echo $stringa3;
```

Questo codice crea tre variabili: `$stringa1`, che contiene la stringa "Ciao ", `$stringa2`, che contiene la stringa "amici!", e `$stringa3`, che unisce le due stringhe precedenti tramite l'operatore `.`. Il risultato sarà la frase "Ciao amici!" stampata a schermo. È importante notare che l'operatore di concatenazione `. `deve essere usato tra le due stringhe per unirle correttamente.

È possibile concatenare più di due stringhe aggiungendo l'operatore `.` tra ognuna di esse. È inoltre possibile concatenare stringhe diverse con altri tipi di dati, come ad esempio con numeri o variabili.

```PHP
$stringa1 = "Il prezzo del prodotto è: ";
$prezzo = 20;
$unità = "€";
$stringa2 = $stringa1 . $prezzo . $unità;
echo $stringa2;
```

In questo esempio, viene creato un messaggio personalizzato utilizzando il contenuto di tre variabili: `$stringa1`, che contiene il testo "Il prezzo del prodotto è: ", `$prezzo`, che ha il valore numerico 20, e `$unità`, che contiene il simbolo "€". L'operatore di concatenazione viene utilizzato per unire tutti questi elementi in un'unica stringa stampata a schermo: "Il prezzo del prodotto è: 20€".

## Approfondimento

Oltre all'operatore `.` di concatenazione, PHP offre anche l'operatore `.=`, che combina la concatenazione con l'assegnazione. Questo significa che è possibile unire una stringa a una variabile senza dover specificare nuovamente il nome della variabile. Ad esempio:

```PHP
$stringa1 = "Il prodotto selezionato è: ";
$prodotto = "smartphone";
$stringa1 .= $prodotto;
echo $stringa1;
```

L'operatore `.= `aggiunge la stringa "smartphone" alla fine di `$stringa1`, ottenendo come risultato "Il prodotto selezionato è: smartphone". Questo è utile soprattutto quando la stringa è dinamica, ovvero il suo contenuto dipende dalle scelte dell'utente o da altri dati variabili.

Infine, è importante notare che la concatenazione può essere considerata una forma di manipolazione delle stringhe. Ciò significa che è possibile utilizzare funzioni come `strlen()` o `substr()` per manipolare la stringa di output prima della concatenazione. Questo offre molte possibilità di personalizzazione e formattazione nel risultato finale.

## Vedi Anche

- [Documentazione ufficiale PHP sulle stringhe](https://www.php.net/manual/en/language.operators.string.php)
- [10 esempi di concatenazione in PHP](https://www.codegrepper.com/code-examples/php/how+to+concatenate+string+in+php)
- [Come manipolare le stringhe in PHP](https://www.tutorialspoint.com/php/php_string_manipulation.htm)