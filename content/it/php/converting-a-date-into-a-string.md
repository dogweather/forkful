---
title:    "PHP: Convertire una data in una stringa"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Perché
Convertire una data in una stringa è un'operazione comune nella programmazione PHP. Questa funzionalità è utile quando si desidera visualizzare una data in un formato specifico o interagire con altri sistemi che richiedono una definizione precisa della data.

## Come fare
Per convertire una data in una stringa, è necessario utilizzare la funzione `date()` di PHP. Questa funzione accetta due parametri: il formato della data desiderato e la data da convertire. Ad esempio, per convertire la data corrente in una stringa nel formato "giorno/mese/anno", si può utilizzare il seguente codice:

```
<?php
echo date("d/m/Y");
?>

```
Output: 26/03/2021

Per ulteriori opzioni di formato della data, si può fare riferimento alla documentazione ufficiale di PHP sulla funzione `date()`.

## Approfondimento
È importante notare che la funzione `date()` restituisce la data nel fuso orario del server web. Se si desidera utilizzare un fuso orario diverso, è possibile impostarlo utilizzando la funzione `date_default_timezone_set()`. È anche possibile utilizzare la funzione `strtotime()` per convertire una stringa in una data prima di formattarla con la funzione `date()`.

Inoltre, bisogna prestare attenzione al formato della data che si desidera utilizzare. Alcuni caratteri utilizzati nella formattazione della data possono essere interpretati in modi diversi a seconda del sistema operativo o del linguaggio di programmazione utilizzato. Si consiglia di fare sempre riferimento alla documentazione ufficiale di PHP per essere sicuri del formato corretto.

## Vedi anche
- Documentazione ufficiale di PHP sulla funzione `date()`: https://www.php.net/manual/en/function.date.php
- Documentazione ufficiale di PHP sulla funzione `date_default_timezone_set()`: https://www.php.net/manual/en/function.date-default-timezone-set.php
- Documentazione ufficiale di PHP sulla funzione `strtotime()`: https://www.php.net/manual/en/function.strtotime.php