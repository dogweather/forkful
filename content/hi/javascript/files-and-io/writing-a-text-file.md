---
title:                "एक टेक्स्ट फ़ाइल लिखना"
aliases: - /hi/javascript/writing-a-text-file.md
date:                  2024-02-03T19:28:58.321103-07:00
model:                 gpt-4-0125-preview
simple_title:         "एक टेक्स्ट फ़ाइल लिखना"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/writing-a-text-file.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
JavaScript में एक टेक्स्ट फाइल लिखना अक्सर डेटा को एक सरल, पढ़ने योग्य प्रारूप में बनाने और सहेजने से संबंधित होता है, जिसका उपयोग लॉगिंग, उपयोगकर्ता इनपुट का निर्यात, या कॉन्फ़िगरेशन प्रयोजनाओं के लिए किया जाता है। यह कार्यक्षमता उन अनुप्रयोगों के लिए महत्वपूर्ण है जिन्हें अनुप्रयोग प्रक्रिया की अवधि से आगे डेटा को स्थायी रूप से रखने की आवश्यकता होती है, जिससे एक तरीका प्रदान करती है जानकारी को संग्रहीत करने और बाद में पुन: प्राप्त या साझा करने का।

## कैसे करें:
Node.js वातावरण में, आप टेक्स्ट फाइलें लिखने के लिए निर्मित `fs` (File System) मॉड्यूल का उपयोग कर सकते हैं। यह उदाहरण एक फाइल में टेक्स्ट लिखने का असिंक्रोनस तरीका दिखाता है:

```javascript
const fs = require('fs');

const data = 'Hello, World! This is text to be written into a file.';

fs.writeFile('example.txt', data, (err) => {
  if (err) {
    throw err;
  }
  console.log('File has been written.');
});
```

नमूना आउटपुट:
```
File has been written.
```

सिंक्रोनस फाइल लिखने के लिए, `writeFileSync` का उपयोग करें:
```javascript
try {
  fs.writeFileSync('example.txt', data);
  console.log('File has been written.');
} catch (error) {
  console.error('Error writing file:', error);
}
```

आधुनिक वेब ब्राउज़रों में, File System Access API फाइलों को पढ़ने और लिखने की क्षमता पेश करता है। हालांकि, इसका उपयोग उपयोगकर्ता की अनुमतियों के अधीन होता है। यहाँ एक फाइल बनाने और उसमें लिखने का तरीका है:

```javascript
if ('showSaveFilePicker' in window) {
  const handle = await window.showSaveFilePicker();
  const writable = await handle.createWritable();
  await writable.write('Hello, World! This is browser text file writing.');
  await writable.close();
}
```

अधिक जटिल परिस्थितियों या बड़ी फाइलों के साथ काम करते समय, आप ब्राउज़रों के लिए `FileSaver.js` जैसे तृतीय-पक्ष पुस्तकालयों का चयन कर सकते हैं:

```html
<script src="https://cdnjs.cloudflare.com/ajax/libs/FileSaver.js/2.0.2/FileSaver.min.js"></script>
<script>
  const blob = new Blob(["Hello, World! This is text from FileSaver.js."], {type: "text/plain;charset=utf-8"});
  saveAs(blob, "example.txt");
</script>
```

याद रखें, क्लाइंट-साइड पर फाइलें लिखना (ब्राउज़रों में) सुरक्षा चिंताओं के कारण सीमित है, और उपयोगकर्ता की स्थानीय डिस्क पर सहेजने की आवश्यकता वाला कोई भी संचालन आमतौर पर उनकी स्पष्ट अनुमति की आवश्यकता होगी।
