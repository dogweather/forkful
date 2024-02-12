---
title:                "टेस्ट लिखना"
date:                  2024-02-03T19:32:12.950070-07:00
model:                 gpt-4-0125-preview
simple_title:         "टेस्ट लिखना"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
Python में परीक्षण लिखना आपके कोड की सहीता की पुष्टि करने के लिए स्वचालित स्क्रिप्ट्स बनाने की प्रक्रिया शामिल है। प्रोग्रामर ऐसा इसलिए करते हैं ताकि वे यह सुनिश्चित कर सकें कि उनके फ़ंक्शंस या क्लासेस विभिन्न परिस्थितियों में अनुमानित के अनुसार ठीक से काम कर रहे हों, जिससे गलतियों को जल्दी पकड़ा जा सके और आसान रखरखाव व पुनर्गठन की सुविधा होती है।

## कैसे:
Python में परीक्षण लिखने के लिए एक निर्मित मॉड्यूल `unittest` उपलब्ध है। आप इसे एक सरल फ़ंक्शन का परीक्षण करने के लिए इस प्रकार उपयोग कर सकते हैं:

```python
import unittest

def add(a, b):
    return a + b

class TestAddFunction(unittest.TestCase):
    def test_add(self):
        self.assertEqual(add(1, 2), 3)
        self.assertEqual(add(-1, 1), 0)
        self.assertNotEqual(add(10, 2), 12, "होना चाहिए 12")

if __name__ == '__main__':
    unittest.main()
```

जब आप इस परीक्षण स्क्रिप्ट को चलाएंगे, तो आपको परिणाम दिखाई देंगे कि आपके परीक्षण पास हुए हैं (या फेल हुए हैं)।

अधिक आधुनिक और अभिव्यक्तिपूर्ण परीक्षणों के लिए, आप एक थर्ड-पार्टी लाइब्रेरी जैसे `pytest` का उपयोग कर सकते हैं। सबसे पहले, आपको इसे pip का उपयोग करके स्थापित करना होगा:

```shell
pip install pytest
```

फिर, आप किसी भी चीज़ को सबक्लास न करते हुए अपने परीक्षण एक सरल तरीके से लिख सकते हैं:

```python
# इसे test_with_pytest.py नाम की फाइल में सेव करें
def add(a, b):
    return a + b

def test_add():
    assert add(1, 2) == 3
    assert add(-1, 1) == 0
    assert add(10, 2) != 12, "होना चाहिए 12"
```

`pytest` के साथ अपने परीक्षणों को चलाने के लिए, सरलता से करें:

```shell
pytest test_with_pytest.py
```

आपको pytest की ओर से आपके परीक्षण परिणामों को दर्शाती हुई आउटपुट दिखाई देगी।