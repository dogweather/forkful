---
title:                "Att skicka en http-begäran"
html_title:           "Go: Att skicka en http-begäran"
simple_title:         "Att skicka en http-begäran"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/powershell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Vad och varför?
Att skicka en HTTP-förfrågan är processen att begära data från en specifik resurs på internet. Programmerare gör det för att interagera med webbtjänster och hämta data.

## Hur gör jag:
Låt oss skicka en GET-förfrågan i PowerShell:

```PowerShell
$uri = 'http://httpbin.org/get'
$response = Invoke-RestMethod -Uri $uri -Method Get
$response
```
Och det här är hur utdatan ser ut:
```PowerShell
args : @{test=123}
headers : @{Host=http://httpbin.org;...}
url : http://httpbin.org/get?test=123
```
För att skicka en POST-förfrågan, gör så här:

```PowerShell
$uri = 'http://httpbin.org/post'
$body = @{
    'key1' = 'value1'
    'key2' = 'value2'
}
$response = Invoke-RestMethod -Uri $uri -Method Post -Body $body
$response
```
Och så ser svaret ut:
```PowerShell
{
    "args": {},
    "data": "",
    "files": {},
    "form": {
        "key1": "value1",
        "key2": "value2"
    },...
}
```
## Djupdykning

HTTP-förfrågan introducerades ursprungligen med HTTP 0.9 år 1991. Men det var med HTTP 1.0 (1996) och 1.1 (1997) som metoder som GET, POST, PUT och DELETE blev allmänt använda. 

Det finns alternativ till PowerShell för att skicka HTTP-förfrågningar, exempelvis Curl (en kommandoradsverktyg) och olika programmeringsspråk som Python och JavaScript. Valet beror på vad du känner dig mest bekväm med och vad dina behov är. 

När det gäller implementationen konverterar Invoke-RestMethod HTTP-svaret till ett användbart PowerShell-objekt, vilket sparar tid och förenklar bearbetningen.

## Se Även 
[Hur man använder cURL](https://developers.curl.se/docs/http-requests)
[Powershell: About_Remote](https://docs.microsoft.com/en-us/powershell/scripting/learn/remoting?view=powershell-7.1)
[HTTP: The Protocol Every Web Developer Must Know](https://www.ntu.edu.sg/home/ehchua/programming/webprogramming/http_basics.html)