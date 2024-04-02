---
date: 2024-01-20 18:02:25.913153-07:00
description: "Att skicka en HTTP-beg\xE4ran med grundl\xE4ggande autentisering inneb\xE4\
  r att du f\xF6rser en server med anv\xE4ndarnamn och l\xF6senord f\xF6r att bevisa\
  \ din identitet.\u2026"
lastmod: '2024-03-13T22:44:37.996063-06:00'
model: gpt-4-1106-preview
summary: "Att skicka en HTTP-beg\xE4ran med grundl\xE4ggande autentisering inneb\xE4\
  r att du f\xF6rser en server med anv\xE4ndarnamn och l\xF6senord f\xF6r att bevisa\
  \ din identitet.\u2026"
title: "Skicka en HTTP-f\xF6rfr\xE5gan med Basic-autentisering"
weight: 45
---

## Vad & Varför?
Att skicka en HTTP-begäran med grundläggande autentisering innebär att du förser en server med användarnamn och lösenord för att bevisa din identitet. Programmerare gör detta för att säkra åtkomst till resurser på servern, så att bara behöriga användare får tillträde.

## Hur man gör:
Skicka en HTTP-begäran med `curl` i PHP. Här använder vi `CURLOPT_USERPWD` för att lägga till autentiseringsuppgifterna.

```php
<?php
$curl = curl_init();

curl_setopt($curl, CURLOPT_URL, "https://exempel.se/data");
curl_setopt($curl, CURLOPT_RETURNTRANSFER, 1);
curl_setopt($curl, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);
curl_setopt($curl, CURLOPT_USERPWD, "anvandare:losenord");

$response = curl_exec($curl);
if(!$response) {
    die('Error: "' . curl_error($curl) . '" - Code: ' . curl_errno($curl));
}

curl_close($curl);
echo $response;
?>
```

Om servern accepterar dina autentiseringsuppgifter, får du svaret tillbaka. Felaktiga uppgifter ger oftast ett 401 Unauthorized-svar.

## Fördjupning:
Grundläggande autentisering har använts sedan början av webben och är en del av HTTP-protokollet. Det är enkelt men inte det säkraste alternativet, eftersom användarnamn och lösenord skickas i klartext (base64-kodat men enkelt att dekoda). HTTPS bör användas för att kryptera informationen under överföringen.

Andra alternativ inkluderar OAuth och token-baserade autentiseringssystem som erbjuder större säkerhet. Grundläggande autentisering används dock fortfarande för dess enkelhet, särskilt för interna server-till-server-kommunikationer eller där hög säkerhet inte är en prioritet.

Implementeringsdetaljer varierar beroende på vilken HTTP-klient du använder. `curl` är standard i PHP och erbjuder omfattande funktionalitet för att hantera HTTP-förfrågningar och autentisering.

## Se även:
- [PHP cURL Manual](https://www.php.net/manual/en/book.curl.php)
- [HTTP Authentication: Basic and Digest Access Authentication](https://tools.ietf.org/html/rfc2617)
- [Understanding and Using curl](https://ec.haxx.se/)
