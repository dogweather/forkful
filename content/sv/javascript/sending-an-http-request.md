---
title:                "Sända en http-begäran"
html_title:           "Javascript: Sända en http-begäran"
simple_title:         "Sända en http-begäran"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Vad & Varför? 
Att skicka en HTTP förfrågan är en vanlig uppgift för programmerare. Det är helt enkelt begreppet för att skicka en begäran till en server för att hämta data eller utföra en åtgärd. Detta är en grundläggande del av många webbapplikationer och API:er.

## Hur man:
För att skicka en HTTP förfrågan i Javascript, behöver du ett sätt att kommunicera med en server. Det finns flera sätt att göra detta, men ett populärt val är att använda det inbyggda XMLHttpRequest-objektet. Nedan finns ett enkelt exempel på hur det kan implementeras:

```Javascript
const xhr = new XMLHttpRequest();
xhr.open("GET", "https://api.example.com/users");
xhr.send();

xhr.onreadystatechange = function() {
    if (this.readyState == 4 && this.status == 200) {
        console.log(xhr.responseText);
    }
}
```

I detta exempel skickar vi en GET-förfrågan till API:et för att hämta en lista med användare. Vi lyssnar också på xhr-objektets readyState för att kontrollera om förfrågan har slutförts och kontrollerar status för att säkerställa att det är en lyckad förfrågan innan vi loggar svaret till konsolen.

## Deep Dive:
Historiskt sett, var XMLHttpRequest det primära sättet att skicka HTTP förfrågningar i Javascript, men i modern utveckling har det blivit allt vanligare att använda fetch API:et, som tillhandahåller ett enklare och smidigare gränssnitt för att skicka och hantera förfrågningar.

Exempelvis ser en GET-förfrågan med fetch API ut så här:

```Javascript
fetch("https://api.example.com/users")
    .then(response => response.json())
    .then(data => {
        console.log(data);
    })
    .catch(error => console.log(error));
```

Här använder vi istället en inbyggd metod för fetch-funktionen för att skicka förfrågan och sedan hämta och hantera svaret. Detta är bara ett av många alternativ för att skicka HTTP förfrågningar i Javascript.

## See Also:
Det finns många resurser för att lära sig mer om att skicka HTTP förfrågningar i Javascript, inklusive dokumentation och tutorials online. Här är några att utforska:

- [MDN - XMLHttpRequest](https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest)
- [MDN - Fetch API](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API)
- [W3Schools - XMLHttpRequest Tutorial](https://www.w3schools.com/xml/xml_http.asp)
- [W3Schools - Fetch Tutorial](https://www.w3schools.com/js/js_api_fetch.asp)