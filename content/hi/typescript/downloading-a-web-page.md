---
title:                "वेब पन्ना को डाउनलोड करना"
html_title:           "TypeScript: वेब पन्ना को डाउनलोड करना"
simple_title:         "वेब पन्ना को डाउनलोड करना"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

### Kyun
Web page ko download karna bahut zaroori ho sakta hai agar aap kisi website ke code ya content ko analyze karna chahte hain, ya fir offline access ke liye.

### Kaise Karein
```TypeScript
// Import the 'https' module
import https from 'https';

// Make a GET request to the desired URL
https.get('https://example.com', (res) => {
  // Store the received data in a string
  let data = '';
  
  // Receive data in chunks and append to the string
  res.on('data', (chunk) => {
    data += chunk;
  });
  
  // Once all data has been received, log it to the console
  res.on('end', () => {
    console.log(data);
  });
  
  // Handle any errors that may occur
  res.on('error', (err) => {
    console.log(err);
  });
});
```

Output:
```html
<!DOCTYPE html>
<html>
  <head>
    <title>Example Domain</title>
    <meta charset="utf-8" />
    <meta http-equiv="Content-type" content="text/html; charset=utf-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1" />
  </head>
  
  <body>
    <h1>Example Domain</h1>
    <p>This domain is for use in illustrative examples in documents. You may use this domain in literature without prior coordination or asking for permission.</p>
    <p><a href="https://www.iana.org/domains/example">More information...</a></p>
  </body>
</html>
```

### Gehri Jhaank
Web page ko download karte waqt, aapko kuch cheezein dhyan mein rakhti hui chahiye. Ek baat ka dhyaan rakhein ki koi bhi website ke content aur code ko download karna us website ki permission ke bina na karein. Agar aap iske against koi bhi rules ko todainge, toh aap legal problems ka shikaar ho sakte hain. Ek aur baat ka dhyaan rakhein ki website ya server ki load ke liye aap excessive requests na bhejein, kyunki yeh server ko slow kar sakta hai aur band bhi kar sakta hai.

### Dekhen Bhi
[Youtube video on downloading a web page using TypeScript](https://www.youtube.com/watch?v=f_GTPJpi0ic) <br>
[Official TypeScript documentation on making HTTP requests](https://www.typescriptlang.org/docs/handbook/2/generic-functions.html)