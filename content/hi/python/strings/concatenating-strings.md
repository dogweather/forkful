---
title:                "स्ट्रिंग को जोड़ना"
aliases: - /hi/python/concatenating-strings.md
date:                  2024-01-20T17:35:54.369837-07:00
model:                 gpt-4-1106-preview
simple_title:         "स्ट्रिंग को जोड़ना"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/concatenating-strings.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
स्ट्रिंग्स को जोड़ना मतलब उन्हें एक के बाद एक करके एकसाथ लगाना है। प्रोग्रामर्स डाटा को फ़ॉर्मेट करने या यूजर इंटरफ़ेस में जानकारी दिखाने के लिए इसका उपयोग करते हैं।

## कैसे करें:
```python
# साधारण तरीका
greeting = "नमस्ते"
name = "आलोक"
message = greeting + ", " + name + "!"
print(message)  # नमस्ते, आलोक!

# स्पेस के साथ जोड़ना
full_greeting = " ".join([greeting, name, "!"])
print(full_greeting)  # नमस्ते आलोक !

# f-string का उपयोग
f_string_message = f"{greeting}, {name}!"
print(f_string_message)  # नमस्ते, आलोक!
```

## गहराई से जानकारी:
पहले प्रोग्रामर '+' ऑपरेटर का इस्तेमाल करके स्ट्रिंग्स जोड़ते थे, जो अब भी वैलिड है। Python 3.6 से, f-strings स्ट्रिंग्स को जोड़ने का एक पसंदीदा तरीका बन गया, क्योंकि ये तेज़ और पढ़ने में आसान होते हैं। `join()` मेथड तब उपयोगी होता है जब आपको बहुत सारे स्ट्रिंग एलिमेंट्स को एक पैटर्न के साथ जोड़ना होता है, जैसे कि एक लिस्ट एलिमेंट्स को स्पेस या किसी दूसरे चिन्ह से अलग करना।

## सम्बंधित जानकारी:
- Python के आधिकारिक दस्तावेज़ में f-strings: https://docs.python.org/3/reference/lexical_analysis.html#formatted-string-literals
- `join()` मेथड के बारे में और जानकारी: https://docs.python.org/3/library/stdtypes.html#str.join
- '+ vs. Join' पर परफॉर्मेंस की तुलना: https://stackoverflow.com/questions/19334374/python-concatenating-strings-vs-join
