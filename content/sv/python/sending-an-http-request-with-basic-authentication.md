---
title:                "Sända en http-begäran med grundläggande autentisering"
html_title:           "Python: Sända en http-begäran med grundläggande autentisering"
simple_title:         "Sända en http-begäran med grundläggande autentisering"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Varför
Att skicka en HTTP-förfrågan med grundläggande autentisering är ett enkelt sätt att skydda din webbapplikation från oönskat åtkomst. Genom att använda en kombination av användarnamn och lösenord i din förfrågan kan du säkerställa att endast auktoriserade användare får tillgång till din resurs.

## Hur man gör
För att skicka en HTTP-förfrågan med grundläggande autentisering i Python, följ dessa steg:

1. Importera "requests" biblioteket: ```Python
import requests```
2. Definiera ditt användarnamn och lösenord: ```Python
username = "användarnamn"
password = "lösenord"```
3. Skapa en förfrågan med grundläggande autentisering: ```Python
response = requests.get("https://example.com/resurs", auth=(username, password))```
4. Kolla svaret från servern: ```Python
print(response.status_code) #200 betyder att förfrågan lyckades
print(response.text) #visar innehållet i responsen```

Om du behöver skicka förfrågningar med mer avancerade autentiseringsmetoder, kan du använda "requests.auth" modulen för att implementera det.

## Djupdykning
Vid autentisering med HTTP-användarnamn och lösenord, skickas dina autentiseringsuppgifter i klartext. Detta innebär att det inte är den säkraste metoden för autentisering, och det rekommenderas att använda HTTPS istället för HTTP för att kryptera din förfrågan. Därför är det viktigt att endast använda grundläggande autentisering på resurser som inte innehåller känslig information.

## Se även
- Requests bibliotekets officiella dokumentation: https://requests.readthedocs.io/en/master/
- En introduktion till HTTP-baserad autentisering: https://www.w3.org/Protocols/rfc2616/rfc2616-sec11.html