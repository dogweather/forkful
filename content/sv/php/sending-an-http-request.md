---
title:                "PHP: Att skicka en http-förfrågan"
simple_title:         "Att skicka en http-förfrågan"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/sending-an-http-request.md"
---

{{< edit_this_page >}}

##Varför

Att skicka HTTP-förfrågningar är en viktig del av många webbutvecklingsprojekt. Det är ett sätt att få data från en annan server, använda data från ett API eller göra en åtkomst kontroll för att avgöra vilka användare som är tillåtna att använda en viss resurs. Det är också ett sätt att interagera med andra webbtjänster och skapa dynamiska webbapplikationer.

##Så här gör du

För att skicka en HTTP-förfrågan i PHP använder man funktionen `curl_init()`. Denna funktion returnerar ett cURL-hanteringsobjekt som man sedan kan använda för att konfigurera och utföra förfrågningar.

```PHP
<?php
// Skapa cURL-hanteringsobjekt
$ch = curl_init();

// Ange URL till den server som förfrågan ska skickas till
curl_setopt($ch, CURLOPT_URL, "http://www.example.com/api/data");

// Sätt önskad åtgärd för förfrågan (GET, POST, PUT, DELETE, etc.)
curl_setopt($ch, CURLOPT_CUSTOMREQUEST, "GET");

// Utför förfrågan och lagra resultatet i en variabel
$result = curl_exec($ch);

// Stäng cURL-resursen
curl_close($ch); 

// Skriv ut resultatet
echo $result;
?>
```

Resultatet av denna förfrågan skulle vara data från den anropade URL:en, som man sedan kan använda för att bygga sin webbapplikation.

##Djupdykning

Man kan också använda cURL-hanteringsobjektet för att konfigurera andra parametrar och inställningar, såsom autentisering, huvuden (headers) och cookies. Man kan också lägga till callback-funktioner för att hantera eventuella fel eller för att få tillgång till ytterligare information om förfrågan.

Det finns också alternativa sätt att skicka HTTP-förfrågningar i PHP, såsom att använda funktionen `file_get_contents()` eller bibliotek som Guzzle och HTTP_Request.

## Se även

- [cURL PHP-dokumentation](https://www.php.net/manual/en/book.curl.php)
- [Guzzle bibliotek](http://docs.guzzlephp.org/en/stable/)
- [HTTP_Request bibliotek](https://pear.php.net/package/HTTP_Request)