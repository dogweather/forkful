---
title:                "पाठ की खोज और प्रतिस्थापन"
html_title:           "Bash: पाठ की खोज और प्रतिस्थापन"
simple_title:         "पाठ की खोज और प्रतिस्थापन"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

टेक्स्ट खोजना और बदलना (Searching and Replacing text) यहाँ में हम एक वाक्यांश (जैसे के एक शब्द) को खोजते हैं और इसे किसी अन्य वाक्यांश से बदल देते हैं। प्रोग्रामर्स इसे इसलिए करते हैं ताकि वे अनावश्यक डेटा को हटा सकें और इसे आवश्यक डेटा से बदल सकें। 

## कैसे करें:

Python में आप `str.replace()` इस्तेमाल करके एक स्ट्रिंग में टेक्स्ट को खोजने और बदलने का कार्य कर सकते हैं। 

```python
text = "मैं Python सीख रहा हूँ।"
new_text = text.replace("Python", "Java")
print(new_text)
```

आउटपुट:

```python
"मैं Java सीख रहा हूँ।"
```

## गहराई में:

टेक्स्ट खोजने और बदलने की क्रिया का इतिहास कॉम्प्यूटर साइंस के शुरुआती दिनों से ही जुड़ा हुआ है। आधुनिक प्रोग्रामिंग भाषाओं ने इसे और आसान बना दिया है, लेकिन यह समस्या पाठ्य संसाधनों (text resources) के साथ काम करने वाले लगभग सभी सॉफ्टवेयर प्रोजेक्ट्स में आम है। 

Python के अलावा, अन्य भाषाओं में भी इस तरह के समान संरचना (structure) और कार्यक्षमता (functionality) पाए जाते हैं। जैसे कि JavaScript में `.replace()` और Java में `.replaceAll()`.

यह  Python में टेक्स्ट को पता लगाने और बदलने का एक आसान तरीका है, लेकिन यदि आपको जटिल खोज बदलाव करने हों, तो आपको Python के रेगुलर एक्सप्रेशन मॉड्यूल `re` का इस्तेमाल करना होगा। 

## अन्य स्रोतों के लिए देखें:

1. Python डॉक्स स्ट्रिंग मेथड्स:
   [https://docs.python.org/3/library/stdtypes.html#string-methods](https://docs.python.org/3/library/stdtypes.html#string-methods)
2. Python रेगुलर एक्सप्रेशन डॉक्स:
   [https://docs.python.org/3/library/re.html](https://docs.python.org/3/library/re.html)
3. Java `String.replace()` मेथड: 
   [https://www.geeksforgeeks.org/replace-a-character-in-a-string-in-java/](https://www.geeksforgeeks.org/replace-a-character-in-a-string-in-java/)
4. JavaScript `String.replace()` मेथड:
   [https://www.w3schools.com/jsref/jsref_replace.asp](https://www.w3schools.com/jsref/jsref_replace.asp)