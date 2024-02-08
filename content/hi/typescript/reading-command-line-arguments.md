---
title:                "कमांड लाइन आर्गुमेंट्स पढ़ना"
date:                  2024-01-20T17:57:35.680899-07:00
model:                 gpt-4-1106-preview
simple_title:         "कमांड लाइन आर्गुमेंट्स पढ़ना"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
कमांड लाइन आर्ग्यूमेंट पढ़ना यानी कमांड लाइन से इनपुट लेना। प्रोग्रामर्स इसे इसलिए करते हैं, क्योंकि यह ज़रूरी होता है कार्यक्रम को फ्लेक्सिबल बनाने के लिए।

## How to: (कैसे करें:)
TypeScript में कमांड लाइन आर्ग्यूमेंट पढ़ने के लिए `process.argv` एरे का इस्तेमाल करते हैं। ये बेसिक उदाहरण दिखाता है:

```TypeScript
// filename: greet.ts
const args = process.argv.slice(2); // पहले दो एलीमेंट्स को हटाता हैं
console.log(`नमस्ते ${args[0]}!`);
```

कमांड लाइन से इसे ऐसे चलाएं:

```bash
$ tsc greet.ts
$ node greet.js John
नमस्ते John!
```

## Deep Dive (गहराई से जानकारी)
पहले लोग बहुत सारे टूल्स इस्तेमाल करते थे, पर `process.argv` डायरेक्ट Node.js से आ रहा है।

बेहतर समझ और एरर हैंडलिंग के लिए `yargs` या `commander` जैसे लाइब्रेरीज का भी इस्तेमाल होता है।

TypeScript में interface या class के साथ types को डिफाइन करके आर्ग्यूमेंट्स की वैलिडीटी चेक कर सकते हैं। जैसे:

```TypeScript
interface CommandLineOptions {
  name: string;
  // आप और भी ऑप्शन्स ऐड कर सकते हैं।
}

// ...कोड के बाकी हिस्से में आप चेक कर सकते हैं कि 'CommandLineOptions' के तहत वैल्यूज सही हैं या नहीं।
```

## See Also (देखने के लिए)
- Node.js `process.argv` documentation: [Node.js Docs](https://nodejs.org/docs/latest/api/process.html#process_process_argv)
- `yargs` library: [yargs GitHub](https://github.com/yargs/yargs)
- `commander` library: [Commander GitHub](https://github.com/tj/commander.js)

प्रोग्रामिंग की दुनिया में और सीखने के लिए इन लिंक्स को चेक करते रहें।
