---
title:                "मानक त्रुटि में लिखना"
html_title:           "Arduino: मानक त्रुटि में लिखना"
simple_title:         "मानक त्रुटि में लिखना"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
स्टैंडर्ड एरर में लिखना त्रुटी और डायग्नोस्टिक संदेशों के लिए एक स्ट्रीम है। प्रोग्रामर्स इसे डिबगिंग को आसान बनाने और स्टैंडर्ड आउटपुट को सिर्फ वैध डेटा के लिए रखने के लिए इस्तेमाल करते हैं।

## How to: (कैसे करें:)
```TypeScript
// स्टैंडर्ड एरर में एक त्रुटि संदेश लिखें
process.stderr.write('यह एक त्रुटि संदेश हैं\n');

// स्टैंडर्ड आउटपुट और स्टैंडर्ड एरर का इस्तेमाल करते हुए उदाहरण
console.log('स्टैंडर्ड आउटपुट पर संदेश');
console.error('स्टैंडर्ड एरर पर संदेश');

// Sample Output:
// स्टैंडर्ड आउटपुट पर संदेश
// स्टैंडर्ड एरर पर संदेश
```

## Deep Dive (गहन जानकारी)
"standard error" (stderr) एक UNIX concept है जो कि लगभग 1980 के दशक से उपयोग में है। इसका उपयोग एरर मैसेजेस और वॉर्निंग को स्टैंडर्ड आउटपुट से अलग रखने के लिए किया जाता है। Node.js में, `process.stderr` एक लिखने योग्य स्ट्रीम है जिसका इस्तेमाल आप सीधे `console.error` के जरिए या `process.stderr.write` का उपयोग करते हुए कर सकते हैं। `console.error` फंक्शन `stderr` स्ट्रीम को और आसानी से उपयोग करने के लिए उपलब्ध करवाता है और इसमें बिल्ट-इन स्ट्रिंग फॉर्मेटिंग भी होती है।

## See Also (यह भी देखें)
- Node.js Documentation on Console: https://nodejs.org/api/console.html
- Node.js Documentation on process.stderr: https://nodejs.org/api/process.html#process_process_stderr
- A Guide to Node.js Logging: https://www.twilio.com/blog/guide-node-js-logging
