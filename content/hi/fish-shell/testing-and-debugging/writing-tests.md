---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:27.559553-07:00
description: "\u0915\u0948\u0938\u0947: Fish \u092E\u0947\u0902 \u0915\u093F\u0938\
  \u0940 \u0905\u0928\u094D\u092F \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\
  \u092E\u093F\u0902\u0917 \u092A\u0930\u094D\u092F\u093E\u0935\u0930\u0923 \u0915\
  \u0940 \u0924\u0930\u0939 \u092C\u093F\u0932\u094D\u091F-\u0907\u0928 \u092A\u0930\
  \u0940\u0915\u094D\u0937\u0923 \u092B\u094D\u0930\u0947\u092E\u0935\u0930\u094D\u0915\
  \ \u0928\u0939\u0940\u0902 \u0939\u094B\u0924\u093E \u0939\u0948\u0964 \u0939\u093E\
  \u0932\u093E\u0902\u0915\u093F, \u0906\u092A \u0906\u0938\u093E\u0928 \u092A\u0930\
  \u0940\u0915\u094D\u0937\u0923 \u0938\u094D\u0915\u094D\u0930\u093F\u092A\u094D\u091F\
  \u094D\uFFFD \u0932\u093F\u0916 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902 \u091C\
  \u094B \u0906\u092A\u0915\u0947\u2026"
lastmod: '2024-04-05T21:53:55.011822-06:00'
model: gpt-4-0125-preview
summary: "Fish \u092E\u0947\u0902 \u0915\u093F\u0938\u0940 \u0905\u0928\u094D\u092F\
  \ \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u093F\u0902\u0917 \u092A\
  \u0930\u094D\u092F\u093E\u0935\u0930\u0923 \u0915\u0940 \u0924\u0930\u0939 \u092C\
  \u093F\u0932\u094D\u091F-\u0907\u0928 \u092A\u0930\u0940\u0915\u094D\u0937\u0923\
  \ \u092B\u094D\u0930\u0947\u092E\u0935\u0930\u094D\u0915 \u0928\u0939\u0940\u0902\
  \ \u0939\u094B\u0924\u093E \u0939\u0948\u0964 \u0939\u093E\u0932\u093E\u0902\u0915\
  \u093F, \u0906\u092A \u0906\u0938\u093E\u0928 \u092A\u0930\u0940\u0915\u094D\u0937\
  \u0923 \u0938\u094D\u0915\u094D\u0930\u093F\u092A\u094D\u091F\u094D\uFFFD \u0932\
  \u093F\u0916 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902 \u091C\u094B \u0906\u092A\
  \u0915\u0947 \u092B\u093C\u0902\u0915\u094D\u0936\u0928\u094D\u0938 \u0915\u0947\
  \ \u0935\u094D\u092F\u0935\u0939\u093E\u0930 \u0915\u0940 \u091C\u093E\u0901\u091A\
  \ \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F \u0926\u093E\u0935\u0947\
  \ (assertions) \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0924\u0947\
  \ \u0939\u0948\u0902\u0964 \u0907\u0938\u0915\u0947 \u0905\u0924\u093F\u0930\u093F\
  \u0915\u094D\u0924, \u0906\u092A `fishtape` \u091C\u0948\u0938\u0947 \u0924\u0940\
  \u0938\u0930\u0947 \u092A\u0915\u094D\u0937 \u0915\u0947 \u0909\u092A\u0915\u0930\
  \u0923\u094B\u0902 \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0915\
  \u0947 \u0905\u0927\u093F\u0915 \u0938\u092E\u0917\u094D\u0930 \u092A\u0930\u0940\
  \u0915\u094D\u0937\u0923 \u0938\u0941\u0907\u091F \u0915\u093E \u0932\u093E\u092D\
  \ \u0909\u0920\u093E \u0938\u0915\u0924\u0947 \u0939\u0948\u0902\u0964."
title: "\u091F\u0947\u0938\u094D\u091F \u0932\u093F\u0916\u0928\u093E"
weight: 36
---

## कैसे:
Fish में किसी अन्य प्रोग्रामिंग पर्यावरण की तरह बिल्ट-इन परीक्षण फ्रेमवर्क नहीं होता है। हालांकि, आप आसान परीक्षण स्क्रिप्ट्� लिख सकते हैं जो आपके फ़ंक्शन्स के व्यवहार की जाँच करने के लिए दावे (assertions) का उपयोग करते हैं। इसके अतिरिक्त, आप `fishtape` जैसे तीसरे पक्ष के उपकरणों का उपयोग करके अधिक समग्र परीक्षण सुइट का लाभ उठा सकते हैं।

### उदाहरण 1: बेसिक टेस्ट स्क्रिप्ट
आइये Fish में एक बुनियादी फंक्शन से शुरू करते हैं जो दो संख्याओं का योग कैलकुलेट करता है:

```fish
function add --description 'Add two numbers'
    set -l sum (math $argv[1] + $argv[2])
    echo $sum
end
```

इस फ़ंक्शन के लिए आप एक बेसिक टेस्ट स्क्रिप्ट इस प्रकार लिख सकते हैं:

```fish
function test_add
    set -l result (add 3 4)
    if test $result -eq 7
        echo "test_add passed"
    else
        echo "test_add failed"
    end
end

test_add
```

इस स्क्रिप्ट को चलाने पर आउटपुट होगा:

```
test_add passed
```

### उदाहरण 2: Fishtape का उपयोग करना
एक अधिक मजबूत परीक्षण समाधान के लिए, आप `fishtape`, एक TAP उत्पन्न करने वाला टेस्ट रनर जो Fish के लिए है, का उपयोग कर सकते हैं।

सबसे पहले, यदि आपने पहले `fishtape` इंस्टॉल नहीं किया है तो इंस्टॉल करें:

```fish
fisher install jorgebucaran/fishtape
```

अगला, अपने `add` फंक्शन के लिए एक टेस्ट फाइल बनाएं, उदाहरण के लिए, `add_test.fish`:

```fish
test "Adding 3 and 4 yields 7"
    set result (add 3 4)
    echo "$result" | fishtape
end
```

टेस्ट चलाने के लिए, निम्नलिखित कमांड का उपयोग करें:

```fish
fishtape add_test.fish
```

नमूना आउटपुट इस प्रकार दिख सकता है:

```
TAP version 13
# Adding 3 and 4 yields 7
ok 1 - test_add passed
```

यह आपको बताता है कि परीक्षण सफलतापूर्वक पास हो गया। `fishtape` आपको अधिक विस्तृत परीक्षणों को संरचना देने और सूचनात्मक आउटपुट प्रदान करने की सुविधा देता है, जिससे डीबगिंग आसान हो जाती है और आपकी Fish स्क्रिप्ट्स के लिए व्यापक परीक्षण कवरेज प्रदान करता है।
