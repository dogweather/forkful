---
title:                "टेस्ट लिखना"
aliases:
- hi/fish-shell/writing-tests.md
date:                  2024-02-03T19:31:27.559553-07:00
model:                 gpt-4-0125-preview
simple_title:         "टेस्ट लिखना"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

Fish Shell में परीक्षण (Tests) लिखने का मतलब है ऐसी स्क्रिप्ट्स बनाना जो स्वचालित रूप से आपके कोड को चलाएं और इसके व्यवहार को अपेक्षित परिणामों के खिलाफ मान्य करें। यह अभ्यास महत्वपूर्ण है क्योंकि यह सुनिश्चित करता है कि आपकी शेल स्क्रिप्ट्स जैसा अनुमानित है, वैसे ही काम कर रही हैं, गलतियों को जल्दी पकड़ता है और रखरखाव को आसान बनाता है।

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
