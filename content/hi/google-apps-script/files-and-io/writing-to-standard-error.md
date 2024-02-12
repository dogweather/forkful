---
title:                "मानक त्रुटि में लिखना"
date:                  2024-02-01T22:10:58.733567-07:00
model:                 gpt-4-0125-preview
simple_title:         "मानक त्रुटि में लिखना"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/google-apps-script/writing-to-standard-error.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

प्रोग्रामिंग भाषाओं में स्टैंडर्ड एरर (stderr) में लिखना गलतियों के संदेशों और नैदानिक जानकारी को स्टैंडर्ड आउटपुट (stdout) से अलग धारा में डायरेक्ट करने के बारे में होता है। प्रोग्रामर इसे सामान्य प्रोग्राम आउटपुट से गलती संदेशों को अलग करने के लिए करते हैं, जिससे डीबगिंग और लॉग विश्लेषण अधिक सरल हो जाता है।

## कैसे:

Google Apps Script, Google Apps प्लेटफॉर्म में हल्के-फुल्के एप्लीकेशन विकास के लिए एक स्क्रिप्टिंग भाषा होते हुए, Node.js या Python में पाए जाने वाले `console.error()` जैसे सीधे बिल्ट-इन फंक्शन को प्रदान नहीं करती है जो stderr में लिखने के लिए होता है। हालांकि, आप Google Apps Script की लॉगिंग सेवाओं या कस्टम एरर हैंडलिंग का उपयोग करके इस व्यवहार का अनुकरण कर सकते हैं जिससे गलतियों के आउटपुट को प्रबंधित और अलग किया जा सके।

### उदाहरण: एरर मेसेज के लिए `Logger` का प्रयोग

```javascript
function logError() {
  try {
    // एक गलती का अनुकरण करें
    const result = 1 / 0;
    if(!isFinite(result)) throw new Error("शून्य से विभाजन का प्रयास");
  } catch (e) {
    // लॉग्स में गलती संदेश लिखें
    Logger.log('Error: ' + e.message);
  }
}
```

जब आप `logError()` चलाते हैं, यह गलती संदेश को Google Apps Script के लॉग में लिख देगा, जिसे आप `View > Logs` द्वारा देख सकते हैं। यह ठीक से stderr नहीं है, लेकिन यह समान उद्देश्य की सेवा करता है, स्टैंडर्ड आउटपुट से गलती लॉग्स को अलग करता है।

### उन्नत नैदानिक लॉगिंग

अधिक उन्नत डीबगिंग और गलती लॉगिंग के लिए, आप Stackdriver Logging का उपयोग कर सकते हैं, जिसे अब Google Cloud's Operations Suite के रूप में जाना जाता है।

```javascript
function advancedErrorLogging() {
  try {
    // जानबूझकर एक गलती पैदा करें
    const obj = null;
    const result = obj.someProperty;
  } catch (e) {
    console.error('पाई गई गलती: ', e.toString());
  }
}
```

यह गलती संदेश को Stackdriver Logging के पास निर्देशित करेगा, जहां इसे एक गलती-स्तर के लॉग के रूप में प्रबंधित किया जाता है। ध्यान दें कि Stackdriver/Google Cloud’s Operations Suite एकीकरण `Logger` की तुलना में एक अधिक सूक्ष्म और खोजयोग्य लॉगिंग समाधान प्रदान करता है।

## गहराई से समझना

Google Apps Script में एक समर्पित `stderr` धारा की कमी इसके स्वभाव और उत्पत्ति को दर्शाती है, जो एक क्लाउड-आधारित स्क्रिप्टिंग भाषा के रूप में है, जहां पारंपरिक कंसोल या टर्मिनल-आधारित आउटपुट (जैसे कि stdout और stderr) कम प्रासंगिक होते हैं। ऐतिहासिक रूप से, Google Apps Script को Google Apps की कार्यक्षमता को सरल स्क्रिप्ट्स के साथ बढ़ाने के लिए डिजाइन किया गया था, जो पेचीदा प्र�
