---
title:    "Javascript: स्ट्रिंग्स को जोड़ना।"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

## क्यों

कोडिंग के क्षेत्र में, जब हम दो या अधिक स्ट्रिंग्स को एक साथ जोड़ते हैं, तो इसे स्ट्रिंग-स्ट्रिंग कॉनकेनेशन (string-string concatenation) कहा जाता है। यह काम काफी उपयोगी है जब हम अपने प्रोग्राम में स्ट्रिंग्स को दोहराना या संपादित करना चाहते हैं। 

## कैसे करें

```Javascript
// स्ट्रिंग्स को जोड़ने का तरीका
let string1 = "मेरा ";
let string2 = "नाम";
let string3 = string1 + string2;
console.log(string3);

// उत्पाद:
// मेरा नाम
```

जैसा कि आप देख सकते हैं, हम ने `+` ऑपरेटर का उपयोग किया है ताकि हम वांछित स्ट्रिंग को बना सकें। यह तरीका दो स्ट्रिंग्स का जोड़ने के लिए काफी सरल है। आप अपने इच्छानुसार अनेक स्ट्रिंग्स को एक साथ जोड़ सकते हैं। 

## गहराई से जाने

जब हम `+` ऑपरेटर का उपयोग करके स्ट्रिंग्स को जोड़ते हैं, तो वास्तव में हम दोनों स्ट्रिंग्स को एक स्ट्रिंग वेरिएबल में संयोजित करते हैं। जब हम स्ट्रिंग्स को जोड़ते हैं, तो वे आमतौर पर टाइपस्ट्रिंग के फार्मेटिंग के लिए मिलता हैं। यदि हम दोनों स्ट्रिंग्स को अलग-अलग लेनथ परस्मित करते हैं, तो हम अनुमान लगा सकते हैं कि स्ट्रिंग्स को जोड़ते समय, आखिरी स्ट्रिंग के पहले का नामबिनद्द होगा। 

## देखें भी

- [JavaScript Strings](https://www.w3schools.com/js/js_strings.asp)
- [JavaScript String Concat() Method](https://www.w3schools.com/jsref/jsref_concat_string.asp)
- [Concatenation in JavaScript](https://www.geeksforgeeks.org/concatenation-in-javascript/)