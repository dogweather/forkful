---
title:    "TypeScript: अस्थायी फाइल बनाना"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## क्यों

असामान्य स्थितियों में, आपको अपने कार्य को संचालित करने के लिए अस्थायी फ़ाइलों की आवश्यकता हो सकती है। इन अस्थायी फ़ाइलों का उपयोग स्थायी फ़ाइल्स बनाने के लिए तो नहीं किया गया है, इसलिए उसे बाद में हटाया जा सकता है।

## कैसे करें

```TypeScript
// अस्थायी फाइल बनाने के लिए फांक्शन
function createTempFile(): void {
  let fileName: string = "temp-file.txt";
  // अस्थायी फाइल के लिए एक अनुच्छेद बनाएं
  let fileContent: string = "यह आवेदन स्थायी फ़ाइल नहीं है, और उसे बाद में हटा सकते हैं।";
  // फाइल लिखें
  fs.writeFileSync(fileName, fileContent);
  // फ़ाइल बनाई गई है!
  console.log(fileName + " फ़ाइल बना दी गई है।");
}
```

आउटपुट:

temp-file.txt फ़ाइल बना दी गई है।

## गहरी तलाश

अस्थायी फाइलों को बनाने के लिए, आपको निरंतर मूल्यों के साथ फ़ाइल नामों को रखने की आवश्यकता हो सकती है। आप यह भी निर्धारित कर सकते हैं कि आप अपनी अस्थायी फ़ाइलों को कहां स्थानित करें। आप उन्हें आंतरिक संगठन या किसी अन्य सर्वर पर स्थिर फाइलों से भिन्न रख सकते हैं।

## देखें भी

[TypeScript फ़ाइल लेखन और पढ़ना](https://www.typescriptlang.org/docs/handbook/basic-types.html)