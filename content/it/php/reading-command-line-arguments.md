---
title:    "PHP: Lettura degli argomenti della riga di comando."
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Perché

Se sei un programmatore PHP, probabilmente hai sentito parlare di "argomenti da riga di comando". Ma perché dovresti interessarti a questa funzionalità? In breve, leggere gli argomenti da riga di comando può aiutarti a creare script più dinamici e facilmente personalizzabili per soddisfare le esigenze dei tuoi utenti.

## Come Fare

Leggere gli argomenti da riga di comando in PHP è sorprendentemente semplice. Utilizzando la funzione interna `getopt()` e il simbolo `$argv`, è possibile accedere facilmente a tutti gli argomenti passati a uno script PHP da riga di comando.

```PHP
$opzioni = getopt("ho:");
var_dump($opzioni);

php script.php -h -o output.txt
```

Questo codice stamperà l'array delle opzioni come `[ "h" => true, "o" => "output.txt" ]`.

## Approfondimento

Ma come funziona esattamente la funzione `getopt()`? E quali sono le sue opzioni disponibili? Qui di seguito è riportato un esempio che utilizza l'opzione `-o` e il valore di output come argomento.

```PHP
$opzioni = getopt("o:");
$opzioneO = $opzioni['o'];

if (!empty($opzioneO)) {
    echo $opzioneO;
}

php script.php -o output.txt
```

In questo caso, l'output verrà stampato direttamente a schermo e sarà "output.txt".

## Vedi Anche

Ecco alcuni link utili per approfondire l'argomento:

- [La documentazione ufficiale di PHP su getopt()](https://www.php.net/manual/en/function.getopt.php)
- [Un tutorial su howtogeek.com](https://www.howtogeek.com/465973/how-to-use-command-line-arguments-in-php/)
- [Una spiegazione dettagliata su geekytheory.com](https://www.geekytheory.com/parametros-linux-linea-comando-php/)

Buon coding!