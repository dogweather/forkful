---
title:                "टेस्ट सिर्जन करना"
date:                  2024-02-03T18:16:53.090872-07:00
model:                 gpt-4-0125-preview
simple_title:         "टेस्ट सिर्जन करना"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

Go में परीक्षण लिखना आपके एप्लिकेशन की कार्यक्षमता और व्यवहार को मान्य करने वाले छोटे, संभालने योग्य कोड के टुकड़ों का निर्माण शामिल करता है। प्रोग्रामर विभिन्न स्थितियों में उनका कोड अपेक्षित रूप से काम करे, रीफैक्टरिंग को सहज बनाने, और प्रतिगमनों को रोकने में मदद करने के लिए परीक्षण लिखते हैं।

## कैसे करें:

Go में, परीक्षण आम तौर पर उसी पैकेज में लिखे जाते हैं जिसमें वे परीक्षण करते हैं। परीक्षणों वाली फाइलों का नाम `_test.go` सुफ़िक्स के साथ दिया गया है। परीक्षण फ़ंक्शन होते हैं जो एक पॉइंटर को `testing.T` ऑब्जेक्ट (`testing` पैकेज से) के रूप में एक तर्क के रूप में लेते हैं, और वे `t.Fail()`, `t.Errorf()`, आदि जैसे तरीकों को कॉल करके विफलता का संकेत देते हैं।

`math.go` में परिभाषित `Add` फंक्शन के लिए एक सरल परीक्षण का उदाहरण:
```go
// math.go
package math

func Add(x, y int) int {
    return x + y
}
```

परीक्षण फ़ाइल `math_test.go`:
```go
package math

import "testing"

func TestAdd(t *testing.T) {
    result := Add(1, 2)
    expected := 3
    if result != expected {
        t.Errorf("Add(1, 2) = %d; want %d", result, expected)
    }
}
```

अपने परीक्षणों को `go test` कमांड के साथ उसी डायरेक्टरी में चलाएं जहां आपकी परीक्षण फाइलें हैं। सफल परीक्षण का संकेत देने वाला नमूना आउटपुट इस प्रकार दिखेगा:

```
PASS
ok      example.com/my/math 0.002s
```

विभिन्न इनपुट और आउटपुट संयोजनों का प्रभावी ढंग से परीक्षण करने के लिए, परीक्षण मामलों को प्रतिनिधित्व करने वाले संरचनाओं के स्लाइस को परिभाषित करें:

```go
func TestAddTableDriven(t *testing.T) {
    var tests = []struct {
        x        int
        y        int
        expected int
    }{
        {1, 2, 3},
        {2, 3, 5},
        {-1, -2, -3},
    }

    for _, tt := range tests {
        testname := fmt.Sprintf("%d+%d", tt.x, tt.y)
        t.Run(testname, func(t *testing.T) {
            ans := Add(tt.x, tt.y)
            if ans != tt.expected {
                t.Errorf("got %d, want %d", ans, tt.expected)
            }
        })
    }
}
```

## गहन विश्लेषण

Go परीक्षण फ्रेमवर्क, जो Go 1 के साथ भाषा के साथ ही पेश किया गया था, को Go टूलचेन के साथ सहज रूप से एकीकृत करने के लिए डिज़ाइन किया गया था, जो सॉफ़्टवेयर विकास में साधारणता और कुशलता पर Go के जोर को दर्शाता है। अन्य भाषाओं के कुछ परीक्षण फ्रेमवर्कों के विपरीत जो बाहरी लाइब्रेरियों या जटिल सेटअपों पर निर्भर करते हैं, Go का बिल्ट-इन `testing` पैकेज परीक्षण लिखने और चलाने का एक सीधा तरीका प्रदान करता है।

Go के परीक्षण करने के दृष्टिकोण में एक दिलचस्प पहलू कन्वेंशन ओवर कॉन्फ़िगरेशन सिद्धांत है जिसे यह अपनाता है, जैसे कि फ़ाइल नामकरण पैटर्न (`_test.go`) और बाहरी निर्भरताएं के ऊपर मानक लाइब्रेरी कार्यक्षमताओं का उपयोग। यह न्यूनतम दृष्टिकोण डेवलपर्स को परीक्षण लिखने के लिए प्रोत्साहित करता है, क्योंकि प्रवेश की बाधा कम है।

जबकि Go की बिल्ट-इन परीक्षण सुविधाएँ बहुत सारे क्षेत्रों को कवर करती हैं, कुछ स्थितियाँ हैं जहाँ तीसरे पक्ष के उपकरण या फ्रेमवर्क अधिक कार्यक्षमता प्रदान कर सकते हैं, जैसे कि मॉक जनरेशन, फ़ज़ परीक्षण, या व्यवहार-संचालित विकास (BDD) शैली के परीक्षण। Testify या GoMock जैसी लोकप्रिय लाइब्रेरियाँ Go की मानक परीक्षण क्षमताओं को पूरक करती हैं, अधिक अभिव्यक्तिशील दावों या मॉक जनरेशन क्षमताओं की पेशकश करती हैं, जो कई निर्भरताओं वाले जटिल एप्लिकेशनों में विशेष रूप से उपयोगी हो सकती हैं।

इन विकल्पों के अस्तित्व के बावजूद, मानक Go परीक्षण पैकेज इसकी सादगी, प्रदर्शन, और भाषा और टूलचेन के साथ कसकर एकीकरण के कारण Go में परीक्षण के लिए आधारशिला बना रहता है। डेवलपर्स इसे तीसरे पक्ष के उपकरणों के साथ बढ़ाने का विकल्प चुनें या नहीं, Go परीक्षण फ्रेमवर्क कोड की गुणवत्ता और विश्वसनीयता सुनिश्चित करने के लिए एक मजबूत आधार प्रदान करता है।