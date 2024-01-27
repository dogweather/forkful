---
title:                "मानक त्रुटि में लिखना"
date:                  2024-01-19
html_title:           "Arduino: मानक त्रुटि में लिखना"
simple_title:         "मानक त्रुटि में लिखना"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)
"स्टैण्डर्ड एरर" एक स्ट्रीम है जिसमें प्रोग्राम अपनी गलतियाँ लिखता है। प्रोग्रामर्स इसका इस्तेमाल ऐसे मेसेज दिखाने के लिए करते हैं जो यूजर्स को एरर के बारे में जानकारी देते हैं या जो लॉगिंग सिस्टम में एरर्स रिकॉर्ड करते हैं।

## कैसे करें? (How to:)
```javascript
// सिंपल मेसेज स्टैण्डर्ड एरर पर प्रिंट करना
process.stderr.write('यह एक एरर मेसेज है.\n');

// कंसोल.एरर का इस्तेमाल करना
console.error('और यह भी एक एरर मेसेज है.');

// एरर ऑब्जेक्ट को स्टैण्डर्ड एरर पर प्रिंट करना
const errorObj = new Error('कुछ गलत हो गया है!');
console.error(errorObj);
```
सैम्पल आउटपुट:
```
यह एक एरर मेसेज है.
और यह भी एक एरर मेसेज है.
Error: कुछ गलत हो गया है!
    at Object.<anonymous> (C:\path\to\script.js:6:15)
    at Module._compile (node:internal/modules/cjs/loader:1218:14)
    ...
```

## गहराई से जानकारी (Deep Dive)
पारंपरिक UNIX फिलॉसफी में, स्टैण्डर्ड आउटपुट (stdout) और स्टैण्डर्ड एरर (stderr) अलग क्यों रखे गए हैं, इसका एक तार्किक आधार है: यह एरर मेसेजेज को नॉर्मल आउटपुट से अलग करता है। JavaScript में `console.error()` और `process.stderr.write()` दो विकल्प हैं जो डेवलपर्स को स्टैण्डर्ड एरर स्ट्रीम पर डाटा लिखने देते हैं। इन दोनों मेथड्स का कार्य लगभग एक समान होता है, लेकिन `console.error()` मेथड अधिक उपयोगी होता है क्योंकि यह स्टैक ट्रेस समेत ऑब्जेक्ट्स को भी प्रिंट करने में सक्षम है।

## सम्बंधित स्रोत (See Also)
- [नोड.जेएस डॉक्स पर process.stderr](https://nodejs.org/api/process.html#processstderr)
- [MDN Web Docs पर console.error()](https://developer.mozilla.org/en-US/docs/Web/API/Console/error)
- [यूनिक्स प्रोग्राम्मिंग फिलॉसफी](https://en.wikipedia.org/wiki/Unix_philosophy#Do_One_Thing_and_Do_It_Well)
