---
date: 2024-01-20 17:59:52.173820-07:00
description: "How to: Anv\xE4nd `fetch` f\xF6r att skicka en enkel GET-beg\xE4ran."
lastmod: '2024-03-13T22:44:38.290273-06:00'
model: gpt-4-1106-preview
summary: "Anv\xE4nd `fetch` f\xF6r att skicka en enkel GET-beg\xE4ran."
title: "Skicka en http-f\xF6rfr\xE5gan"
weight: 44
---

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
