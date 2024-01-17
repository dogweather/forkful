---
title:                "Sända en http-förfrågan med grundläggande autentisering"
html_title:           "Javascript: Sända en http-förfrågan med grundläggande autentisering"
simple_title:         "Sända en http-förfrågan med grundläggande autentisering"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Vad & Varför?

När en Javascript-programmerare skickar en HTTP-förfrågan med grundläggande autentisering, innebär det att man skickar ett användarnamn och lösenord med förfrågan för att verifiera sin identitet. Detta används vanligtvis för att få tillgång till skyddade resurser på en webbplats eller tjänst.

Programmerare använder grundläggande autentisering för att säkerställa att endast auktoriserade användare kan få tillgång till vissa resurser. Det ger en grundläggande nivå av säkerhet utan att kräva mer komplexa autentiseringsmetoder.

## Så här gör du:

```Javascript
const username = "användarnamn";
const password = "lösenord";

// Skapa en ny förfrågan med grundläggande autentisering
const request = new XMLHttpRequest();
request.open("GET", "https://www.example.com/api/resource", true);
request.setRequestHeader("Authorization", `Basic ${btoa(username + ":" + password)}`);

// Skicka förfrågan och hantera svaret
request.send();

request.onreadystatechange = function() {
  if (this.readyState === 4 && this.status === 200) {
    // Hantera returvärdet från förfrågan
    console.log(this.responseText);
  }
};
```

I exemplet ovan använder vi den inbyggda btoa-funktionen för att koda användarnamn och lösenord i Base64-format och skicka det i en HTTP-förfrågan. När förfrågan har skickats, kan vi hantera svaret genom att använda den inbyggda XMLHttpRequest-metoden.

## Djupdykning:

Grundläggande autentisering har funnits sedan begynnelsen av internet och används vanligtvis för att skydda resurser som inte kräver hög säkerhet. Det finns dock nackdelar med grundläggande autentisering, som att lösenordet kan ses i klartext om förfrågan avlyssnas.

Alternativen till grundläggande autentisering inkluderar mer avancerade autentiseringsmetoder som OAuth eller användning av API-nycklar. Det är viktigt att noga överväga vilken autentiseringsmetod som bäst passar behoven för din webbapplikation eller tjänst.

För att implementera grundläggande autentisering på servern, kan man använda en servermiljövariabel som kontrollerar användarinformationen mot en databas eller annan autentiseringslösning. Det är också viktigt att se till att servern använder en säker anslutning (HTTPS) för att skydda användarnas uppgifter.

## Se även:

- [XMLHttpRequest-dokumentation](https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest)
- [Autentisering i webbapplikationer](https://www.owasp.org/index.php/Web_Application_Authentication)