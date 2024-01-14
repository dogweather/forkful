---
title:                "Rust: Confronto di due date"
simple_title:         "Confronto di due date"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Perché

Quando si lavora con date in un programma, può essere utile confrontare due date tra loro. Ad esempio, si potrebbe voler sapere se una data è successiva o precedente rispetto a un'altra, o se due date sono uguali. In questo articolo, esploreremo come fare questo in Rust in modo efficiente e preciso.

## Come Fare

Per confrontare due date in Rust, utilizzeremo la libreria standard `chrono`. Iniziamo importando questa libreria nel nostro codice:

```Rust
use chrono::{Datelike, NaiveDate};
```

Successivamente, creiamo due variabili di tipo `NaiveDate` contenenti le date che vogliamo confrontare:

```Rust
let date1 = NaiveDate::from_ymd(2021, 3, 15);
let date2 = NaiveDate::from_ymd(2020, 11, 1);
```

Per confrontare queste due date, utilizzeremo i metodi `partial_cmp` e `eq`. Questi metodi restituiscono un'enumerazione `Option` che rappresenta il risultato del confronto tra le date. Ad esempio:

```Rust
let result = date1.partial_cmp(&date2); // restituisce None se le date non sono confrontabili

if let Some(ordering) = result {
    match ordering {
        Ordering::Greater => println!("La prima data è successiva alla seconda"),
        Ordering::Less => println!("La seconda data è successiva alla prima"),
        Ordering::Equal => println!("Le due date sono uguali"),
    }
}
```
L'output di questo codice sarebbe:

```
La prima data è successiva alla seconda
```

Se vogliamo solo verificare se le due date sono uguali o meno, possiamo utilizzare il metodo `eq`, che restituisce un valore booleano:

```Rust
let result = date1.eq(&date2); // restituisce false se le date sono diverse

if result {
    println!("Le due date sono uguali");
} else {
    println!("Le due date sono diverse");
}
```
L'output di questo codice sarebbe:

```
Le due date sono diverse
```

È anche possibile confrontare solo il giorno, solo il mese o solo l'anno tra due date. Per fare ciò, possiamo utilizzare i metodi `day`, `month` e `year` rispettivamente. Ad esempio, per confrontare solo il giorno tra le due date:

```Rust
let day1 = date1.day();
let day2 = date2.day();

if day1 == day2 {
    println!("I giorni delle due date sono uguali");
} else {
    println!("I giorni delle due date sono diversi");
}
```

L'output di questo codice sarebbe:

```
I giorni delle due date sono diversi
```

## Deep Dive

Il metodo `partial_cmp` utilizza l'ordinamento naturale delle date per confrontarle. Ciò significa che le date più recenti vengono considerate "maggiori" delle date più vecchie. Inoltre, è importante notare che questo metodo usa il "calendario gregoriano" per calcolare le date.

Se si desidera utilizzare un diverso calendario per i confronti tra date, o se si vogliono confrontare date che non utilizzano l'ordinamento naturale (ad esempio, date di nascita), è possibile utilizzare il metodo `compare`.

```Rust
use chrono::{Datelike, NaiveDate, Weekday};

let date1 = NaiveDate::from_ymd(2021, 3, 15);
let date2 = NaiveDate::from_ymd(2020, 11, 1);

let result = date1.compare(&date2); // restituisce Ordering::Equal se le date sono uguali

if let Some(ordering) = result {
    match ordering {
        Ordering::Greater => println!("La prima data è successiva alla seconda"),
        Ordering::Less => println!("La seconda data è successiva alla prima"),
        Ordering::Equal => println!("Le due date sono uguali"),
    }
}
```

L'output di questo codice sarebbe lo stesso del primo esempio, ma utilizzando il metodo `compare` possiamo anche specificare un calendario diverso:

```Rust
use chrono::{Datelike, NaiveDate, Weekday};

let date1 = Na