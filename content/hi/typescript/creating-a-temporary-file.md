---
title:                "अस्थायी फाइल बनाना"
html_title:           "TypeScript: अस्थायी फाइल बनाना"
simple_title:         "अस्थायी फाइल बनाना"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## क्यों

क्या आपने अपने कोड में अस्थायी फाइलों की आवश्यकता महसूस की है? यह फाइलें कोड का एक अंश हो सकती हैं, लेकिन ये आपके कोड को बेहतर और अधिक संगठित बनाती हैं और साथ ही कुछ विशेषताओं को भी सक्षम करतीं हैं।

## कैसे करें

```TypeScript 
import * as fs from 'fs';
import * as path from 'path';

const tempFilePath = path.join(__dirname, 'tempFile.txt');

// अस्थायी फाइल बनाने के लिए नया फ़ाइल निर्माण करें
fs.open(tempFilePath, 'w', (err, fd) => {
  if (err) throw err;
  // अस्थायी फ़ाइल में डेटा लिखें
  fs.write(fd, 'यह एक अस्थायी फ़ाइल है', (err) => {
    if (err) throw err;
    // अस्थायी फ़ाइल बंद करें
    fs.close(fd, (err) => {
      if (err) throw err;
      console.log('अस्थायी फ़ाइल सफलतापूर्वक बनाई गई।');
      // अस्थायी फ़ाइल को हटा दें
      fs.unlink(tempFilePath, (err) => {
        if (err) throw err;
        console.log('अस्थायी फ़ाइल सफलतापूर्वक हटा दी गई।');
      });
    });
  });
});
```

जैसा कि आप देख सकते हैं, हमने शुरू में `fs` और `path` लाइब्रेरी को उपयोग किया है। यह लाइब्रेरी फ़ाइल सिस्टम ऑपरेशन के लिए उपयोगी है। हमने `path.join()` फ़ंक्शन का उपयोग करके एक अस्थायी फ़ाइल के लिए नया पथ बनाया है। फिर हम `fs.open()` फ़ंक्शन का उपयोग करके फ़ाइल को खोलते हैं और इसे `w` रीड और राइट मोड में खोलते हैं। अस्थायी फ़ाइल को बंद या हटाने के लिए हम `fs.close()` और `fs.unlink()` फ़ंक्शन का उपयोग करते हैं। 

## गहराई में जाएं

अस्थायी