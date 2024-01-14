---
title:                "Javascript: Sända en http-begäran med grundläggande autentisering"
simple_title:         "Sända en http-begäran med grundläggande autentisering"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Varför 

I denna bloggpost kommer vi att diskutera hur man kan skicka en HTTP-förfrågan med grundläggande autentisering i Javascript. Detta är ett vanligt problem som många utvecklare stöter på när de arbetar med webbapplikationer. Att skicka en HTTP-förfrågan med grundläggande autentisering är en effektiv och säker metod för att kommunicera med servern och säkerställa att endast auktoriserade användare får åtkomst till viss information.

## Hur man gör det 

I denna sektion kommer vi att gå igenom en kodexempel på hur man skickar en HTTP-förfrågan med grundläggande autentisering i Javascript. Vi kommer att använda fetch API för att utföra förfrågan och ett exempel API för att hämta data. Nedan är en kodexempel på hur man skickar en HTTP-förfrågan med grundläggande autentisering:

```Javascript 
fetch('https://exampleapi.com/data', {
 method: 'GET',
 headers: {
   'Authorization': 'Basic ' + btoa('username:password')  // här krypteras användarnamnet och lösenordet med base64-kodning
 }
})
.then(response => response.json())  // här konverteras svar från servern till JSON-format
.then(data => console.log(data))  // här loggas data i konsolen
.catch(error => console.log(error));  // här fångar vi eventuella fel som kan uppstå

```

Eftersom autentiseringsinformationen är krypterad med base64-kodning är det viktigt att vara medveten om att detta inte är en säker metod för autentisering. För att upprätthålla säkerheten i din applikation bör du utvärdera andra autentiseringsmetoder som OAuth eller JWT.

## Djupgående 

Att skicka en HTTP-förfrågan med grundläggande autentisering i Javascript är relativt enkelt, men det finns några viktiga saker att komma ihåg. Till att börja med måste du alltid se till att SSL-säkerhet är aktiverat på din server för att skydda den autentisering som sker. Detta är särskilt viktigt när man skickar känslig information som användarnamn och lösenord.

En annan viktig punkt att komma ihåg är att autentiseringsinformationen bör kodas med base64 istället för att skickas i klartext. Detta förhindrar att autentiseringsinformationen kan läsas av obehöriga parter.

Sist men inte minst, se till att du aldrig lagrar autentiseringsinformationen på klienten. Detta är en säkerhetsrisk eftersom koden enkelt kan nås och återkodas av en erfaren utvecklare.

## Se även 

- [Tutorial: HTTP Basic Authentication in JavaScript](https://www.taniarascia.com/how-to-connect-to-an-api-with-javascript/)
- [MDN Web Docs: Using Fetch](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API/Using_Fetch)
- [OWASP: Secure Coding Practices](https://owasp.org/www-project-cheat-sheets/cheatsheets/Insecure_Direct_Object_Reference_Prevention_Cheat_Sheet.html)
- [Express.js: HTTP Basic Authentication](https://expressjs.com/en/advanced/best-practice-security.html)