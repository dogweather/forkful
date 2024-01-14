---
title:    "PHP: Lesen von Befehlszeilenargumenten"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

### Warum
Wenn Sie sich schon immer gefragt haben, wie Sie in PHP Befehlszeilenargumente lesen können, dann sind Sie hier genau richtig! In diesem Blogbeitrag werde ich Ihnen zeigen, wie Sie mithilfe von PHP ganz einfach Befehlszeilenargumente lesen können.

### Wie geht's
Um Befehlszeilenargumente in PHP lesen zu können, müssen wir zuerst die Variable `$argv` verwenden. Diese Variable enthält alle Argumente, die an das Skript beim Aufruf übergeben werden. Hier ist ein einfaches Beispiel:

```PHP
$arguments = $argv;
print_r($arguments);
```

Wenn wir nun unser PHP Skript mit Befehlszeilenargumenten aufrufen, erhalten wir eine Ausgabe wie folgt:

```PHP
Array
(
    [0] => /usr/local/bin/php
    [1] => my_script.php
    [2] => argument1
    [3] => argument2
)
```

Wie Sie sehen können, enthält die Variable `$argv` alle übergebenen Argumente als Array. Das erste Element ([0]) ist der Pfad zur ausführenden PHP-Datei, gefolgt von den übergebenen Argumenten.

Sie können nun auf die Argumente wie auf jedes andere Array-Element zugreifen. Zum Beispiel können wir das zweite Argument ([2]) ausgeben, indem wir folgenden Code verwenden:

```PHP
echo "Erstes Argument: " . $arguments[2]; // Output: Erstes Argument: argument1
```

### Tiefenanalyse
Es gibt noch weitere Möglichkeiten, um Befehlszeilenargumente in PHP zu lesen. Wenn Sie beispielsweise wissen möchten, wie viele Argumente übergeben wurden, können Sie die Funktion `count()` verwenden:

```PHP
$argc = count($argv);
echo "Anzahl der Argumente: " . $argc; // Output: Anzahl der Argumente: 4
```

Sie können auch überprüfen, ob ein bestimmtes Argument übergeben wurde, indem Sie die Funktion `in_array()` verwenden:

```PHP
if (in_array("argument3", $arguments)) {
    echo "Das Argument 3 wurde übergeben!";
} else {
    echo "Das Argument 3 wurde nicht übergeben!";
}
```

Wenn Sie Befehlszeilenargumente mit Leerzeichen übergeben möchten, müssen Sie diese in Anführungszeichen setzen. Zum Beispiel:

```bash
php my_script.php "Argument mit Leerzeichen" argument2
```

### Siehe auch
- [PHP Manual - Befehlszeilenparameter](https://www.php.net/manual/de/features.commandline.php)
- [PHP Manual - $argv](https://www.php.net/manual/de/reserved.variables.argv.php)
- [PHP Manual - count()](https://www.php.net/manual/de/function.count.php)
- [PHP Manual - in_array()](https://www.php.net/manual/de/function.in-array.php)