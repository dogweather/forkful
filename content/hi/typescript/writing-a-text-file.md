---
title:                "TypeScript: टेक्स्ट फ़ाइल लिखना"
simple_title:         "टेक्स्ट फ़ाइल लिखना"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## क्यों
टेक्स्ट फ़ाइल लिखने में लोगों को क्यों रुचि हो सकती है, इसके बारे में अन्यों के साथ सांस्कृतिक समन्ध हो सकते हैं।

## कैसे करें
```TypeScript
// एक टेक्स्ट फ़ाइल बनाएं
fs.writeFile("myfile.txt", "यह टेक्स्ट फ़ाइल कोड के साथ स्पष्ट रूप से दो लाइन के साथ बनाई गई है।", (err) => {
    if (err) {
        console.error(err);
        return;
    }
    console.log('टेक्स्ट फ़ाइल सफलतापूर्वक बनाई गई है।');
});
```

```TypeScript
// टेक्स्ट फ़ाइल को पढ़ें
fs.readFile('myfile.txt', 'utf8', (err, data) => {
    if (err) {
        console.error(err);
        return;
    }
    console.log(data);
});
```

इस प्रोसेस को कोड के माध्यम से बहुत आसानी से दोहराया जा सकता है। यह करने के लिए, हम `fs` मॉड्यूल का उपयोग कर सकते हैं जो Node.js के साथ डिफ़ॉल्ट रूप से आता है।

## गहन जाँच
टेक्स्ट फ़ाइल लिखने के लिए, हमें बिना अवलोकन किए एक फ़ाइल को खोलना होगा। इसके बाद, हम उस फ़ाइल को `data` या स्ट्रिंग के माध्यम से पढ़ें और `writeFile` का उपयोग करके उसे संपादित कर सकते हैं। अंत में, हम फ़ाइल को `close()` के माध्यम से समाप्त करें और उसे सेव करें।

## अन्य उपयोगी स्रोत

- [Node.js और TypeScript द्वारा टेक्स्ट फ़ाइल लिखने का वीडियो ट्यूटोरियल](https://www.youtube.com/watch?v=d3RCBW9S3ug)
- [विश्व में सबसे बड़ा JavaScript नेटवर्क - टेक्स्ट फ़ाइल लिखना](https://www.w3schools.com/nodejs/nodejs_filesystem.asp)

## देखें भी
यदि आप TypeScript सीखना और टेक्स्ट फ़ाइल कोडिंग के बार