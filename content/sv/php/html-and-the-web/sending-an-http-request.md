---
date: 2024-01-20 18:00:10.161608-07:00
description: "Hur g\xF6r man: F\xF6r att skicka en HTTP-beg\xE4ran i PHP \xE4r `cURL`\
  \ en vanlig metod. H\xE4r \xE4r ett enkelt skript som g\xF6r en GET-beg\xE4ran."
lastmod: '2024-03-13T22:44:37.993234-06:00'
model: gpt-4-1106-preview
summary: "F\xF6r att skicka en HTTP-beg\xE4ran i PHP \xE4r `cURL` en vanlig metod."
title: "Skicka en http-f\xF6rfr\xE5gan"
weight: 44
---

## Hur gör man:
För att skicka en HTTP-begäran i PHP är `cURL` en vanlig metod. Här är ett enkelt skript som gör en GET-begäran:

```PHP
<?php
$curl = curl_init();

curl_setopt($curl, CURLOPT_URL, "https://api.example.com/data");
curl_setopt($curl, CURLOPT_RETURNTRANSFER, 1);

$response = curl_exec($curl);
if ($response) {
    echo "Data received: " . $response;
} else {
    echo "Request failed: " . curl_error($curl);
}

curl_close($curl);
?>
```

Sample Output:
```
Data received: {"id": 123, "name": "Alice"}
```

Nu ett exempel på en POST-begäran för att skicka data:

```PHP
<?php
$curl = curl_init();

$data = array('id' => '123', 'name' => 'Alice');
$jsonData = json_encode($data);

curl_setopt($curl, CURLOPT_URL, "https://api.example.com/submit");
curl_setopt($curl, CURLOPT_POST, 1);
curl_setopt($curl, CURLOPT_POSTFIELDS, $jsonData);
curl_setopt($curl, CURLOPT_HTTPHEADER, array('Content-Type:application/json'));
curl_setopt($curl, CURLOPT_RETURNTRANSFER, 1);

$response = curl_exec($curl);
if ($response) {
    echo "Server responded: " . $response;
} else {
    echo "Request failed: " . curl_error($curl);
}

curl_close($curl);
?>
```

Sample Output:
```
Server responded: {"status": "success", "message": "Data saved"}
```

## Djupdykning:
Förhistorien börjar i 90-talet när webben växte och behovet av dynamiska begäran ökade. PHP utrustades med olika metoder för att utföra dessa begäran, där `cURL` är den mest robusta.

Alternativ till cURL inkluderar `file_get_contents()` för enklare GET-begäran eller mer moderna bibliotek som `Guzzle`, som tillhandahåller en mer objektorienterad lösning.

När det gäller implementation är det viktigt att hantera fel korrekt, använda timeout för att undvika långa väntetider och säkerställa att data är säkert skickade, speciellt med känslig information.

## Se även:
- [PHP.NET cURL manual](https://www.php.net/manual/en/book.curl.php) – Komplett manual för cURL i PHP.
- [Guzzle documentation](http://docs.guzzlephp.org/en/stable/) – Läs mer om Guzzle-biblioteket.
- [REST API tutorial](https://www.restapitutorial.com/) – För att bättre förstå REST API:er som ofta använder HTTP-begäran.
