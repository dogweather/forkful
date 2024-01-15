---
title:                "Sända en http-begäran med grundläggande autentisering"
html_title:           "Javascript: Sända en http-begäran med grundläggande autentisering"
simple_title:         "Sända en http-begäran med grundläggande autentisering"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Varför

Att skicka en HTTP-förfrågan med grundläggande autentisering är en vanlig uppgift inom webbutveckling. Det är ett säkert och effektivt sätt att verifiera användare och tillhandahålla åtkomst till skyddade resurser på en webbplats eller API. 

## Så här gör du

```Javascript
// Skapa en ny instans av XMLHttpRequest-objektet
var xhr = new XMLHttpRequest();

// Ange URL:en och begäran
xhr.open('GET', 'https://example.com/api', true);

// Ange autentiseringsuppgifter i en sträng på formatet "username:password" och koda den med base64
var encodedCredentials = btoa("username:password");

// Sätt en Authorization-header med det kodade värdet
xhr.setRequestHeader("Authorization", "Basic " + encodedCredentials);

// Skicka begäran och hantera eventuella svar
xhr.onload = function() {
  // Status 200 betyder att begäran lyckades
  if (xhr.status === 200) {
    var response = xhr.responseText;
    console.log(response);
  }
};

// Skicka begäran
xhr.send();
```

**Output:**
```
{
  "id": 1234,
  "username": "john",
  "email": "john@example.com"
}
```

## Djupdykning

Vad händer egentligen bakom kulisserna när en HTTP-förfrågan med grundläggande autentisering skickas? När vi sätter en Authorization-header med det bas64-kodade kombinationen av användarnamn och lösenord, tolkas detta av servern som ett HTTP-grundläggande autentiseringscertifikat. Denna autentisering används vanligtvis tillsammans med en säker HTTPS-anslutning för att skydda känslig information.

## Se även

- [XMLHttpRequest](https://developer.mozilla.org/sv-SE/docs/Web/API/XMLHttpRequest)
- [Base64](https://developer.mozilla.org/sv-SE/docs/Web/API/WindowBase64/btoa)
- [HTTP Basic Authentication](https://developer.mozilla.org/sv-SE/docs/Web/HTTP/Authentication#Basic_authentication_scheme)