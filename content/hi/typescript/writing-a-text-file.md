---
title:                "कम्प्यूटर प्रोग्रामिंग पर एक आलेख टेक्स्ट फाइल लिखना।"
html_title:           "TypeScript: कम्प्यूटर प्रोग्रामिंग पर एक आलेख टेक्स्ट फाइल लिखना।"
simple_title:         "कम्प्यूटर प्रोग्रामिंग पर एक आलेख टेक्स्ट फाइल लिखना।"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## क्यों

कभी-कभी हमें अपनी कोड को साझा करने के लिए साधारण टेक्स्ट फाइलों की आवश्यकता होती है। इसलिए हमें एक अच्छे तरीके से पाठ लिखना सीखने की आवश्यकता होती है।

## कैसे करें

```TypeScript
import * as fs from "fs";

fs.writeFile("sample.txt", "Hello world!", (err) => {
  if (err) {
    console.log(err);
  } else {
    console.log("File written successfully!");
  }
});

```

ऊपर दिए गए कोड साल संस्करण 3.4.1 का हे। साथ ही, हम फ़ाइल `sample.txt` बनाते हैं और "Hello world!" का प्रिंट करते हैं। यदि कोई त्रुटि होती है तो हम कोणो कोनोमा आक्सडस्तन लोग देते हैं।

## गहराई में जाएँ

आप अपनी टेक्स्ट फ़ाइल को संपादित करने के लिए `fs` मॉड्यूल का उपयोग कर सकते हैं, लेकिन यह आपको कुछ अहम पैरामीटर भी पास करने को प्रोत्साहित करेगा। लेकिन, यदि आप `fs.writeFileSync()` का उपयोग करते हैं, तो आपको उन पैरामीटरों को नहीं पास करने की ज़रूरत होती है। इसलिए, यह समर्थन पाठ फाइल लिखने के लिए बहुत अधिक सीमित होता है। इससे आप संपादन के दौरान और अधिक सीमित हो जाते हैं और आपको एक ही समय दौरान बहुत सारी फ़ाइल लिखने की ज़रूरत हो सकती है।।

## और भी देखें

- [डेनो नोडjs की ऑफिशियल वेबसाइट] (https://deno.land)

- [टाइपस्क्रिप्ट और डेनो के बारे में जानने के लिए लेख] (https://auth0.com/blog/denow-typescript-and-javascript-on-the-backend/)