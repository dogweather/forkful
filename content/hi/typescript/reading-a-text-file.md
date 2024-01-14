---
title:                "TypeScript: टेक्स्ट फ़ाइल को अधययन करना"
programming_language: "TypeScript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## क्यों

तोह आपने बनाया एक खास program, लेकिन अब आपको उसमें से पुराना data लेना है। इसके लिए आपके program को एक text file से दूसरे तक का data पढ़ने को प्रोग्रामिंग किये गुनगुनाने की जरूरत है। तोह यह article आपको teach करेगी की कैसे आप TypeScript में text file पढ़ सकते हैं।

## कैसे करें

```TypeScript
import * as fs from "fs";

// file path
const filePath: string = "test.txt";

// read file
fs.readFile(filePath, "utf8", (err: any, data: any) => {
  if (err) throw err;
  console.log(data);
});
```
यह सारा code हमने `fs` module से लिखा है। `fs` module आपको file system से interaction करने की सुविधा देता है। इसे import करने के बाद, हम एक `filePath` variable बनाते हैं जो हमारे text file का path होता है। उसके बाद, हम `fs.readFile()` method को use करके file को read करते हैं। इस method में हम 3 arguments पास करते हैं: file path, encoding और एक callback function। जब file read हो जाता है, callback function को call किया जाता है जिसमें `err` और `data` parameters होते हैं। `err` parameter में error message या `null` हो सकता है, जबकि `data` में file का data होता है। हमने `console.log()` का use करके data को print किया है। आप देख सकते हैं की file के सारे लाइन print हो रहे हैं।

## गहराई में

यदि आपको text file को line by line read करना है, फिर आप `fs.createReadline()` method का use कर सकते हैं। इसमें आपको line by line data को access करने के लिए `readline` object भी मिलता है। आप parameter के रूप में `line` नामक variable का use करके हर लाइन को प्रिंट कर सकते हैं।

एक और interesting method है `fs.readFileSync()` जो मेंशन किया गया हैं। यह `fs.readFile()` की तरह ही काम करता है, लेकिन इसमें आप file को synchronous तरीके से read कर सकते हैं। इससे, program को file के data का इंतज़ार नहीं करना पड़ता और file read होने के बाद ही program आगे बढ़त