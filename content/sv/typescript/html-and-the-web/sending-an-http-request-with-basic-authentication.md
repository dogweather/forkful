---
date: 2024-01-20 18:03:00.738582-07:00
description: "Steg f\xF6r steg: Eventuell utdata."
lastmod: '2024-04-05T21:53:38.983940-06:00'
model: gpt-4-1106-preview
summary: ''
title: "Skicka en HTTP-f\xF6rfr\xE5gan med Basic-autentisering"
weight: 45
---

## Steg för steg:
```TypeScript
import axios from 'axios';

async function fetchDataWithBasicAuth(url: string, username: string, password: string) {
  const token = Buffer.from(`${username}:${password}`).toString('base64');
  try {
    const response = await axios.get(url, {
      headers: {
        'Authorization': `Basic ${token}`
      }
    });
    console.log(response.data);
  } catch (error) {
    console.error('Authentication failed:', error);
  }
}

// Använd funktionen som så här:
const apiUrl = 'https://example.com/data';
const username = 'myUsername';
const password = 'myPassword';
fetchDataWithBasicAuth(apiUrl, username, password);
```

Eventuell utdata:
```
{ "hemligData": "Världen är en ostron." }
```

## Fördjupning:
Basic Authentication är en gammal metod, definierad i HTTP/1.0 och sen uppdaterad i RFC 7617, som kräver att användarnamn och lösenord skickas med varje förfrågan, vilket är mindre säkert jämfört med moderna metoder som OAuth 2.0. Av detta skäl används det sällan för nya applikationer, men det kan fortfarande ses i äldre system eller för enkla integrationer där hög säkerhet inte är prioriterad. När du implementerar Basic Authentication i TypeScript, använd bibliotek som axios för HTTP-förfrågningar och skydda alltid dina källor genom att hålla känslig information som användarnamn och lösenord utanför koden, helst i miljövariabler.

## Se även:
- [MDN - Authorization: Basic](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Authorization)
- [RFC 7617 - The 'Basic' HTTP Authentication Scheme](https://tools.ietf.org/html/rfc7617)
- [Axios documentation](https://axios-http.com/docs/intro)
- [Node.js - Buffer class](https://nodejs.org/api/buffer.html#buffer_class_buffer)
- [dotenv: Ladda miljövariabler från .env filer i Node.js](https://github.com/motdotla/dotenv)
