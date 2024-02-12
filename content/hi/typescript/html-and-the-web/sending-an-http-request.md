---
title:                "HTTP अनुरोध भेजना"
aliases: - /hi/typescript/sending-an-http-request.md
date:                  2024-01-20T18:01:11.598799-07:00
model:                 gpt-4-1106-preview
simple_title:         "HTTP अनुरोध भेजना"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)

HTTP request वेब सर्वर से जानकारी मांगने का एक तरीका है। Programmers इसे डेटा पाने, अपडेट करने, या सर्वर पर कार्रवाई करने के लिए भेजते हैं।

## How to: (कैसे करें:)

TypeScript में HTTP request भेजने के लिए `fetch` API का उपयोग करें। नीचे इसका एक उदाहरण है:

```typescript
async function fetchData(url: string): Promise<void> {
  try {
    const response = await fetch(url);
    if (!response.ok) {
      throw new Error(`HTTP error! status: ${response.status}`);
    }
    const data = await response.json();
    console.log(data);
  } catch (error) {
    console.error('Fetch error:', error.message);
  }
}

fetchData('https://jsonplaceholder.typicode.com/todos/1');
```

जब आप ऊपर के कोड को चलाएंगे, तो आपको कंसोल में JSON डेटा नजर आएगा:

```json
{
  "userId": 1,
  "id": 1,
  "title": "delectus aut autem",
  "completed": false
}
```

## Deep Dive (गहराई में जानकारी:)

HTTP requests का इतिहास 1990s से शुरू होता है, जब World Wide Web नया था। AJAX (Asynchronous JavaScript and XML) तकनीक ने दिखाया कि requests को पेज को रिफ्रेश किए बिना भी भेजा जा सकता है। TypeScript `fetch` API इसी का एक आधुनिक रूप है।

Fetch API के अलावा, `XMLHttpRequest` भी एक विकल्प है, पर ये पुराना और कम सुविधाजनक है। `Axios` जैसे तृतीय-पक्ष लाइब्रेरीज भी हैं जो ज़्यादा सुविधाएँ और ब्राउज़र समर्थन देते हैं।

जब आप `fetch` का उपयोग करते हैं, Promise पर आधारित `async` और `await` शेली कोड को ज्यादा पढ़ने योग्य और संभालने में आसान बनाती है।

## See Also (और देखें:)

- Mozilla MDN Web Docs पर Fetch API: https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API
- हाई परफॉर्मेंस ब्राउज़र नेटवर्किंग (HTTP/2, Fetch, etc.): https://hpbn.co/
- Axios GitHub repository: https://github.com/axios/axios
