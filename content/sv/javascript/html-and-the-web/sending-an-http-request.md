---
aliases:
- /sv/javascript/sending-an-http-request/
date: 2024-01-20 17:59:52.173820-07:00
description: "HTTP-beg\xE4ran \xE4r hur din kod fr\xE5gar efter data or skickar information\
  \ till en server. Programmerare g\xF6r detta f\xF6r att interagera med webbtj\xE4\
  nster, h\xE4mta\u2026"
lastmod: 2024-02-18 23:08:52.161640
model: gpt-4-1106-preview
summary: "HTTP-beg\xE4ran \xE4r hur din kod fr\xE5gar efter data or skickar information\
  \ till en server. Programmerare g\xF6r detta f\xF6r att interagera med webbtj\xE4\
  nster, h\xE4mta\u2026"
title: "Skicka en http-f\xF6rfr\xE5gan"
---

{{< edit_this_page >}}

## What & Why?
HTTP-begäran är hur din kod frågar efter data or skickar information till en server. Programmerare gör detta för att interagera med webbtjänster, hämta innehåll eller skicka användardata.

## How to:
Använd `fetch` för att skicka en enkel GET-begäran:

```Javascript
fetch('https://api.example.com/data')
  .then(response => response.json())
  .then(data => console.log(data))
  .catch(error => console.error('Error:', error));
```

För att posta data, ange metoden och kroppen:

```Javascript
fetch('https://api.example.com/submit', {
  method: 'POST',
  headers: {
    'Content-Type': 'application/json',
  },
  body: JSON.stringify({
    user: 'example',
    password: 'säker'
  }),
})
.then(response => response.json())
.then(data => console.log('Success:', data))
.catch(error => console.error('Error:', error));
```
Sample output:
```
Success: { user: 'example', status: 'logged in' }
```

## Deep Dive
Förr använde vi `XMLHttpRequest` men det var krångligare. `fetch` kom med HTML5 och är nu standarden. Den returnerar `Promises`, vilket förenklar asynkron kod.

Om `fetch` inte räcker finns bibliotek som `axios` eller mer komplexa lösningar som `GraphQL`.

I några fall behöver du hantera CORS-policyer eller använda prestanda-tips som HTTP/2-förpushning. Lär känna dessa scenarier för att bli effektivare.

## See Also
- MDN Web Docs om `fetch`: [MDN fetch documentation](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API/Using_Fetch)
- Information om `XMLHttpRequest`: [MDN XMLHttpRequest](https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest)
- `axios` GitHub repo: [Axios on GitHub](https://github.com/axios/axios)
