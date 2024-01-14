---
title:                "PHP: Pobieranie strony internetowej"
simple_title:         "Pobieranie strony internetowej"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Dlaczego

Nie ważne, czy masz swoją stronę internetową czy też zajmujesz się tworzeniem aplikacji webowych, prędzej czy później będziesz musiał pobrać stronę internetową jako część swojego programu. Niezależnie od celu, dla którego chcesz to zrobić, prawdopodobnie będzie Ci potrzebna wiedza w zakresie programowania w PHP. W tym artykule dowiesz się, jak pobrać stronę internetową za pomocą PHP.

## Jak to zrobić

Pobieranie strony internetowej w PHP jest stosunkowo proste. Musisz tylko użyć funkcji `file_get_contents()` i przekazać jako argument adres URL strony, którą chcesz pobrać. Poniżej znajduje się kod demonstrujący to działanie:

```PHP
<?php
$url = "https://example.com";
$page = file_get_contents($url);
echo $page;
?>
```

Po uruchomieniu tego kodu, powinieneś zobaczyć na stronie wyjściowej treść strony internetowej, którą wybrałeś do pobrania. Jeśli chcesz zapisać pobraną stronę jako plik HTML, możesz wykorzystać funkcję `file_put_contents()`:

```PHP
<?php
$url = "https://example.com";
$page = file_get_contents($url);
file_put_contents('page.html', $page);
?>
```

W tym przykładzie pobierana strona zostanie zapisana jako plik HTML o nazwie "page.html". Możesz także kontrolować sposób, w jaki pobierana jest strona internetowa. Na przykład, jeśli chcesz pobrać tylko nagłówek strony zamiast całej treści, możesz użyć opcji `stream_context_create()`, jak pokazano poniżej:

```PHP
<?php
$url = "https://example.com";
$options = array(
  'http'=>array(
    'method'=>"HEAD",
    'header'=>"User-Agent: PHP"
  )
);
$context = stream_context_create($options);
$fd = fopen($url, 'rb', false, $context);
$meta = stream_get_meta_data($fd);
fclose($fd);
echo "Status: {$meta['wrapper_data'][0]}";
?>
```

## Głębokie zanurzenie

Pobieranie stron internetowych w PHP może być bardziej skomplikowane niż wyżej opisane przykłady. Musisz wziąć pod uwagę wiele czynników, takich jak uwierzytelnienie, obsługa przekierowań, błędy HTTP i wiele innych. Możesz także wykorzystać różne biblioteki i narzędzia, takie jak cURL, do pobierania stron internetowych z większymi możliwościami i kontroli nad pobieranymi danymi.

Teraz, gdy masz podstawową wiedzę na temat pobierania stron internetowych w PHP, możesz spróbować pobrać różne strony i przetestować swoje umiejętności programowania. Pamiętaj jednak, że zawsze powinieneś zachować ostrożność podczas pobierania danych z zewnętrznych stron internetowych, aby uniknąć naruszenia praw autorskich lub ataków na swoją stronę.

## Zobacz także

- [Dokumentacja PHP dotycząca funkcji file_get_contents()](https://www.php.net/manual/en/function.file-get-contents.php)
- [Przewodnik dla początkujących w PHP](https://www.w3schools.com/php/)
- [Strona PHP.net dotycząca cURL](https://www.php.net/manual/en/book.curl.php)