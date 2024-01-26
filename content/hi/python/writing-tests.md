---
title:                "परीक्षण लिखना"
html_title:           "Arduino: परीक्षण लिखना"
simple_title:         "परीक्षण लिखना"
programming_language: "Python"
category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/writing-tests.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
परीक्षण लिखना (Writing tests) यानी अपनी कोड की जांच करने का एक तरीका है जिससे यह सुनिश्चित हो कि कोड सही से काम कर रहा है। प्रोग्रामर इसलिए परीक्षण लिखते हैं ताकि भविष्य में कोड में बदलाव करते समय गलतियों का पता चल सके और कोड की गुणवत्ता बनी रहे।

## कैसे करें:
Python में टेस्ट केसेस बनाने के लिए `unittest` मॉड्यूल एक सामान्य औज़ार है। यहाँ एक सिंपल उदाहरण है:

```python
import unittest

def sum(x, y):
    return x + y

class TestSum(unittest.TestCase):
    def test_sum(self):
        self.assertEqual(sum(3, 4), 7)
        self.assertEqual(sum(-1, 1), 0)
        self.assertEqual(sum(-1, -1), -2)

if __name__ == '__main__':
    unittest.main()
```

जब आप इस कोड को चलाएँगे, तो आउटपुट मिलेगा:

```
...
----------------------------------------------------------------------
Ran 3 tests in 0.001s

OK
```

## गहराई से जानकारी:
परीक्षण लिखने की प्रक्रिया 1950 के दशक से ही प्रयोग में है। `unittest` पायथन की एक बुनियादी यूनिट टेस्टिंग फ्रेमवर्क है लेकिन इसके अलावा `pytest` और `nose` जैसे अन्य उपकरण भी हैं जो और भी सुविधाएँ प्रदान करते हैं। टेस्ट केस लिखते वक्त, असर्ट स्टेटमेंट्स (assert statements) का इस्तेमाल करके कोड के अपेक्षित परिणाम की जाँच की जाती है।

## सम्बंधित सूत्र:
- ऑफिसियल Python `unittest` ट्युटोरियल: https://docs.python.org/3/library/unittest.html
- `pytest` डॉक्युमेंटेशन: https://docs.pytest.org/en/stable/
- `nose2` डॉक्युमेंटेशन: https://docs.nose2.io/en/latest/

इन लिंक्स पर क्लिक करके आप पायथन में परीक्षण लिखने की और भी जानकारी प्राप्त कर सकते हैं।
