---
title:                "Javascript: डीबग आउटपुट प्रिंट करना"
programming_language: "Javascript"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/printing-debug-output.md"
---

{{< edit_this_page >}}

## क्यों

जेसे हम सभी जानते हैं की JavaScript एकी सबसे पोपुलर और सबसे काम के programming languages हे। लेकिन कई बार हमारे कोड में गलती हो जाती है और हमें पता ही नहीं चलता की गलती कहा हुई है या कौनसा कोड कहा execute नहीं हो रहा। ऐसे में हम print करके debug output के माध्यम से हमें error का पता लगा सकते हैं। इसलिए debug output printing दूसरे types के debugging से ज्यादा आसानलिखित-है।

## कैसे करें

```javascript
console.log("Debug output आपको काफी मदद कर सकता है।");
```

जैसे की ऊपर दिखाया गया है की आप console.log() का उपयोग करके आसानी से debug output print कर सकते हैं। कोड में जहा भी आपको लगता है की error हो रहा है उसके ठीक नीचे console.log() को लिखिए और आप गलती को ढूंढ सकते हैं। 

```javascript
let firstName = "John";
let lastName = "Doe";
console.log("firstName: ", firstName);
console.log("lastName: ", lastName);

let fullName = firstName + lastName; // This will cause an error
console.log("fullName: ", fullName);
```

इस उदाहरण में, हमें मालूम है की error कहा है - लास्ट लाइन में fullName वैरियेबल का मूल्य जोडिये कि जगह। और हमारे कंसोल एउटपुट में दिखाया गया है क्योकि हमने उस वैरियेबल का मूल्य print कराया था। आप अपने कोड में जितने बार आपको लगे तबतक आप console.log() को लिख सकते हैं। इससे आपका debugging process बहुत ही सादे हो जाता है।

## गहराई में

Debug output printing का उपयोग करने से आपको गलती को सही करने के लिए काफी मदद मिलती है। इससे आपको पता चलता है की किस लाइन में error हुई है और आप उसे ठीक कर सकते हैं। इससे आपका काम भी आसान हो जाता है क्योकि आपको