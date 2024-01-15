---
title:                "Sända en http-begäran"
html_title:           "PHP: Sända en http-begäran"
simple_title:         "Sända en http-begäran"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Varför

Om du utvecklar en webbapplikation eller arbetar med webb-API:er, kan det vara nödvändigt att skicka en HTTP-förfrågan. Detta är grunden för hur din applikation kommunicerar med andra servrar och får tillgång till data och funktioner som erbjuds.

## Så här gör du

För att skicka en HTTP-förfrågan i PHP behöver du först använda dig av funktionen `curl_init()`. Här anger du den URL du vill skicka förfrågan till och väljer även eventuella parametrar, headers eller metoder.

```PHP
// Skapa en curl-instans
$ch = curl_init("https://example.com/api/users");

// Ange önskad metod
curl_setopt($ch, CURLOPT_CUSTOMREQUEST, "GET");

// Ange önskade headers
curl_setopt($ch, CURLOPT_HTTPHEADER, [
    'Content-Type: application/json',
    'Authorization: Bearer 12345abcde'
]);

// Exekvera förfrågan och spara svar i en variabel
$response = curl_exec($ch);

// Stäng curl-instansen
curl_close($ch);

// Visa svar i konsolen
echo $response;
```

Detta kommer att skicka en GET-förfrågan till URL:en med de angivna headers. Du kan även använda dig av andra metoder, till exempel POST eller PUT, beroende på vad som krävs för att kommunicera med API:et.

## Djupdykning

När du skickar en HTTP-förfrågan finns det flera aspekter att ta hänsyn till. Som utvecklare måste du vara medveten om vilken typ av förfrågan som krävs, vilka headers som måste skickas med, och hur svaret från servern ska hanteras.

Det finns också möjlighet att lägga till eventuella parametrar eller body-data i en förfrågan, beroende på vad API:et förväntar sig. Det är viktigt att läsa dokumentationen noggrant för att se till att förfrågan är korrekt och kan behandlas av servern.

När du tar emot svar från en HTTP-förfrågan är det också viktigt att hantera eventuella felmeddelanden eller statuskoder för att se till att din applikation fungerar som den ska och kan hantera eventuella problem som kan uppstå.

## Se också

- [PHP cURL dokumentation](https://www.php.net/manual/en/book.curl.php)
- [HTTP-request i PHP](https://www.php.net/manual/en/function.curl-init.php)
- [Implementera API:er i PHP-applikationer](https://www.php.net/manual/en/function.curl-exec.php)