---
title:    "PHP: Ottenere la data corrente"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Perché

A volte può sembrare banale, ma la capacità di ottenere la data corrente è un'abilità fondamentale nella programmazione. Conoscere la data e l'ora può essere utile in molte situazioni, come ad esempio nel calcolo di scadenze, nella registrazione di eventi e nell'organizzazione di dati all'interno del tuo programma.

## Come Fare

Per ottenere la data corrente in PHP, è possibile utilizzare la funzione `date()` che restituisce la data attuale formattata secondo le specifiche fornite. Ad esempio, per ottenere la data completa è possibile utilizzare il seguente codice:

```PHP
$today = date("d/m/Y");
echo $today;
```

Questo produrrà un output come "06/01/2022". È possibile utilizzare diversi parametri nella funzione `date()` per ottenere la data in formati diversi.

Per ottenere l'ora attuale, è possibile utilizzare la funzione `time()` che restituisce il numero di secondi trascorsi dal 1 gennaio 1970. Questo è utile se si desidera utilizzare la data per calcolare scadenze o per registrare eventi con precisione.

Per convertire il timestamp restituito dalla funzione `time()` in una data leggibile, è possibile utilizzare la funzione `date()` insieme al parametro `timestamp`:

```PHP
$timestamp = time();
$current_date = date("d/m/Y", $timestamp);
echo $current_date;
```

Questo produrrà lo stesso output del codice precedente.

## Approfondimento

Per controllare la data corrente, PHP utilizza una combinazione di impostazioni del server e della libreria C standard. La funzione `date()` utilizza l'impostazione del fuso orario server per determinare l'ora corrente. Se si desidera utilizzare un fuso orario diverso, è possibile impostarlo utilizzando la funzione `date_default_timezone_set()`.

Inoltre, è possibile utilizzare la classe `DateTime` per ottenere la data corrente in modo più flessibile. Questa classe fornisce più opzioni per la formattazione e la manipolazione della data e dell'ora.

## Vedi Anche
- [Documentazione ufficiale di PHP sulla funzione `date()`](https://www.php.net/manual/en/function.date.php)
- [Informazioni sui fusi orari in PHP](https://www.php.net/manual/en/timezones.php)
- [Documentazione ufficiale di PHP sulla classe `DateTime`](https://www.php.net/manual/en/class.datetime.php)