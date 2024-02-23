---
title:                "नया प्रोजेक्ट शुरू करना"
date:                  2024-02-22T17:31:21.749401-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-02-22, dogweather, reviewed
  - 2024-02-22, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

Python में एक नया प्रोजेक्ट शुरू करना एक संरचित, बनाए रखने योग्य ढांचे को शुरुआत से ही सेट अप करने के बारे में है। प्रोग्रामर यह इसलिए करते हैं ताकि सुनिश्चित किया जा सके कि उनका कोड आसानी से पढ़ने, डीबग करने और कोलैबोरेट करने के लिए हो, खासकर जैसे-जैसे प्रोजेक्ट और उस पर काम करने वाली टीम का आकार बढ़ता है।

## कैसे करें:

### वर्चुअल एन्वायरनमेंट बनाएं
एक वर्चुअल एन्वायरनमेंट एक स्वयं सम्पूर्ण निर्देशिका है जिसमें एक Python प्रोजेक्ट की आवश्यकता वाले पैकेजों का उपयोग करने के लिए सभी आवश्यक एक्ज़ेक्यूटबल्स होते हैं। हर प्रोजेक्ट के लिए एक वर्चुअल एन्वायरनमेंट बनाना सलाह दी जाती है ताकि प्रोजेक्ट डिपेंडेंसीज के बीच संघर्ष से बचा जा सके। `venv` मॉड्यूल का उपयोग करें, जो मानक Python लाइब्रेरी का एक हिस्सा है।

```shell
# 'myproject' को अपने प्रोजेक्ट के नाम से बदलें
python3 -m venv myproject-env
```

वर्चुअल एन्वायरनमेंट को सक्रिय करने के लिए:

Windows पर:
```shell
myproject-env\Scripts\activate.bat
```

Unix या MacOS पर:
```shell
source myproject-env/bin/activate
```

नमूना आउटपुट (आउटपुट OS पर निर्भर करते हुए थोड़ा भिन्न हो सकता है):
```shell
(myproject-env) $
```

### पैकेजेज इंस्टॉल करें
Python के लिए पैकेज इंस्टॉलर `pip` का उपयोग करके पैकेजेज को इंस्टॉल, अपग्रेड, और हटाने के लिए करें। यहां एक लोकप्रिय तृतीय-पक्ष पुस्तकालय, `requests`, को HTTP अनुरोध बनाने के लिए कैसे इंस्टॉल करें है:

```shell
pip install requests
```

नमूना आउटपुट:
```shell
Collecting requests
  Downloading requests-2.25.1-py2.py3-none-any.whl (61 kB)
     |████████████████████████████████| 61 kB 1.3 MB/s
Installing collected packages: requests
Successfully installed requests-2.25.1
```

### प्रोजेक्ट स्ट्रक्चर सेट करना
एक टिपिकल Python प्रोजेक्ट इस तरह दिख सकता है:

```
myproject/
│
├── myproject-env/    # वर्चुअल एन्वायरनमेंट
├── docs/             # दस्तावेज़ीकरण
├── tests/            # यूनिट और इंटीग्रेशन टेस्ट
│   └── __init__.py
├── myproject/        # प्रोजेक्ट स्रोत कोड
│   ├── __init__.py
│   └── main.py
├── setup.py          # प्रोजेक्ट सेटअप फ़ाइल
└── README.md         # प्रोजेक्ट अवलोकन
```

### अपना पहला प्रोग्राम बनाएं
`myproject` निर्देशिका के अंदर एक `main.py` फाइल बनाएं। यहाँ एक साधारण प्रोग्राम का उदाहरण है:

```python
# myproject/myproject/main.py
def greet(name):
    return f"Hello, {name}!"

if __name__ == "__main__":
    print(greet("World"))
```

अपने प्रोग्राम को चलाएँ:

```shell
python myproject/main.py
```

नमूना आउटपुट:
```shell
Hello, World!
```

### बड़े प्रोजेक्ट्स के लिए फ्रेमवर्क का उपयोग करें
खासकर वेब एप्लिकेशन के लिए, बड़े प्रोजेक्ट्स के लिए फ्रेमवर्क जैसे कि Django या Flask अमूल्य हैं। यहां Flask इंस्टॉल करने और एक साधारण "Hello, World" वेब एप्लिकेशन बनाने का तरीका है:

```shell
pip install Flask
```

निम्नलिखित सामग्री के साथ एक `app.py` फाइल बनाएं:

```python
# app.py
from flask import Flask
app = Flask(__name__)

@app.route("/")
def hello_world():
    return "<p>Hello, World!</p>"

if __name__ == "__main__":
    app.run(debug=True)
```

Flask एप्लिकेशन चलाएँ:

```shell
flask run
```

नमूना आउटपुट:
```shell
 * Running on http://127.0.0.1:5000/ (Press CTRL+C to quit)
```

अपने वेब ब्राउज़र में `http://127.0.0.1:5000/` पर नेविगेट करें, और आपको "Hello, World!" संदेश दिखना चाहिए।
