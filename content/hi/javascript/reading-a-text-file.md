---
title:                "Javascript: टेक्स्ट फाइल पढ़ना"
programming_language: "Javascript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/reading-a-text-file.md"
---

{{< edit_this_page >}}

# क्यों

क्या आप जानना चाहते हैं कि कैसे जावास्क्रिप्ट की मदद से एक टेक्स्ट फाइल को पढ़ा जाता है? यह आपको अपने कोडिंग कौशल पर विश्वास देगा और आपको नए स्किल्स सीखने में मदद करेगा। यह आपको अपने कोड को और अधिक व्यावहारिक बनाता है।

# कैसे करें

```Javascript
// टेक्स्ट फाइल के लिए एक HttpServletRequest बनाएं
const request = new XMLHttpRequest();
// फाइल के पथ और प्रकार को सेट करें
request.open('GET', 'example.txt', true);
// डेटा को लोड करते समय को ट्रैक करने के लिए एक onload ईवेंट हैंडलर जोड़ें
request.onload = () => {
  // सफलतापूर्वक डेटा लोड हो जाता है, तो उसे एक वेरिएबल में स्टोर करें
  const data = request.response;
  // डेटा को कंसोल पर प्रिंट करें
  console.log(data);
}
// रिक्वेस्ट को भेजने के लिए स्टार्ट करें
request.send();
```

आप जब यह कोड चलाएंगे, आपको अपनी कंसोल में टेक्स्ट फाइल का सामग्री दिखाई देगी।

# गहराई तक

यदि आप अधिक गहराई तक जानना चाहते हैं, तो आप एक अन्य तरीके से भी टेक्स्ट फाइल को पढ़ सकते हैं। आप एक फ़ंक्शन का उपयोग करके भी डेटा लोड कर सकते हैं।

```Javascript
// एक उदाहरण फ़ंक्शन
function readTextFile(file) {
  const rawFile = new XMLHttpRequest();
  // true को synchronous परिभाषित करने से अनुरोध अपडेट होने तक प्रतीक्षा नहीं करेगा
  rawFile.open("GET", file, false);
  rawFile.onreadystatechange = () => {
    if (rawFile.readyState === 4) {
      if (rawFile.status === 200 || rawFile.status == 0) {
        const allText = rawFile.responseText;
        console.log(allText);
      }
    }
  }
  rawFile.send(null);
}

// फ़ंक्शन को उन फाइलों के साथ बुलाएं जिन्ह