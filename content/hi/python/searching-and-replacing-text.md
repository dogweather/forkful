---
title:    "Python: टेक्स्ट को खोजना और बदलना"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/python/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## क्यों 
आज कल, कंप्यूटर अपने राजमार्ग पर बने हुए हैं, जहां तक समन्धित तकनीक आती है, वस्त्रों और खाना के त्यागों के अनुसार जहां आमतौर पर असमानता होती है, वहाँ समंदर कम नहीं होता है। इसलिए, टेक्स्ट को खोजना और प्रतिस्थापित करना बहुत अंतर्भावी हो सकता है।

## कैसे करें 
```
Python में टेक्स्ट खोजें और प्रतिस्थापित करें
def search_replace(word_replacement, new_word, file_name):
    file = open(file_name, 'r')
    text = file.read()
    file.close()

    new_text = text.replace(word_replacement, new_word)

    file = open(file_name, 'w')
    file.write(new_text)
    file.close()

search_replace('कॉम पाइपिंग', 'पाइथन', 'example.txt')
```

इस कोड ब्लॉक में, हमने `search_replace` नामक एक फ़ंक्शन बनाई है जो दो चीजों को लेती है - `word_replacement` और `new_word`, जो एक विस्तार स्थान Aur एक दूसरे बदलते हैं। हम फ़ाइल नाम `file_name` भी लेते हैं जो हमारी पाठ को खोलने और बनाने के लिए उपयोगित होता है। `search_replace` फ़ंक्शन अपने कार्यक्रम में एक `def` शब्द का उपयोग कर रही है, जो कि यह तब उठाया जाता है जब किसी भी फ़ंक्शन ने कोई संरचना और अपने आप को परिभाषित किया है। आप में उदाहरण के लिए, हमने हमारे `example.txt` फ़ाइल में "कॉम पाइपिंग" को "पाइथन" में बदलने का प्रयास किया है।

## गहराई तक जाईए 
टेक्स्ट खोजने और प्रतिस्थापित करने की यह सुविधा आसानी से पाठित हो सकती है, लेकिन आप इसमें गहराई में जा सकते हैं। आपको किसी मिश्रित स्क्रिप्ट म