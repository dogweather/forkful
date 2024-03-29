---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:10:38.838437-07:00
description: "Google Apps Script (\u0917\u0942\u0917\u0932 \u090F\u092A\u094D\u0938\
  \ \u0938\u094D\u0915\u094D\u0930\u093F\u092A\u094D\u091F - GAS) \u092E\u0947\u0902\
  \ \u092A\u0930\u0940\u0915\u094D\u0937\u0923 \u0932\u093F\u0916\u0928\u093E \u0906\
  \u092A\u0915\u0947 \u0915\u094B\u0921\u094D\u0938 \u0915\u0947 \u0935\u094D\u092F\
  \u0935\u0939\u093E\u0930 \u0915\u0940 \u092A\u0941\u0937\u094D\u091F\u093F \u0915\
  \u0947 \u0932\u093F\u090F \u0938\u094D\u0935\u091A\u093E\u0932\u093F\u0924 \u0938\
  \u094D\u0915\u094D\u0930\u093F\u092A\u094D\u091F\u094D\u0938 \u092C\u0928\u093E\u0928\
  \u0947 \u0915\u0947 \u092C\u093E\u0930\u0947 \u092E\u0947\u0902 \u0939\u0948, \u092F\
  \u0939\u2026"
lastmod: '2024-03-13T22:44:51.519010-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script (\u0917\u0942\u0917\u0932 \u090F\u092A\u094D\u0938 \u0938\
  \u094D\u0915\u094D\u0930\u093F\u092A\u094D\u091F - GAS) \u092E\u0947\u0902 \u092A\
  \u0930\u0940\u0915\u094D\u0937\u0923 \u0932\u093F\u0916\u0928\u093E \u0906\u092A\
  \u0915\u0947 \u0915\u094B\u0921\u094D\u0938 \u0915\u0947 \u0935\u094D\u092F\u0935\
  \u0939\u093E\u0930 \u0915\u0940 \u092A\u0941\u0937\u094D\u091F\u093F \u0915\u0947\
  \ \u0932\u093F\u090F \u0938\u094D\u0935\u091A\u093E\u0932\u093F\u0924 \u0938\u094D\
  \u0915\u094D\u0930\u093F\u092A\u094D\u091F\u094D\u0938 \u092C\u0928\u093E\u0928\u0947\
  \ \u0915\u0947 \u092C\u093E\u0930\u0947 \u092E\u0947\u0902 \u0939\u0948, \u092F\u0939\
  \u2026"
title: "\u091F\u0947\u0938\u094D\u091F \u0932\u093F\u0916\u0928\u093E"
---

{{< edit_this_page >}}

## क्या और क्यों?

Google Apps Script (गूगल एप्स स्क्रिप्ट - GAS) में परीक्षण लिखना आपके कोड्स के व्यवहार की पुष्टि के लिए स्वचालित स्क्रिप्ट्स बनाने के बारे में है, यह सुनिश्चित करने के लिए कि वे विभिन्न स्थितियों के अधीन उम्मीद के अनुरूप प्रदर्शन करें। प्रोग्रामर इसे जल्दी बगों को पकड़ने, कोड की गुणवत्ता में सुधार और आसान अपडेट्स और रखरखाव को सुविधाजनक बनाने के लिए करते हैं।

## कैसे करें:

हालांकि Google Apps Script में कुछ अन्य प्रोग्रामिंग वातावरणों की तरह एक निर्मित परीक्षण फ्रेमवर्क नहीं है, आप अभी भी सरल GAS कार्यों का उपयोग करके या `QUnit` जैसी बाहरी परीक्षण लाइब्रेरियों को एकीकृत करके परीक्षण लिख और चला सकते हैं। यहाँ एक बुनियादी उदाहरण दिया गया है जिसमें आपके स्क्रिप्ट में एक और फंक्शन के परीक्षण के लिए एक सरल GAS फंक्शन का उपयोग किया गया है:

```javascript
function add(a, b) {
  return a + b;
}

function testAdd() {
  var result = add(2, 3);
  if (result !== 5) {
    throw new Error("Test failed: add(2, 3) should be 5, but was " + result);
  } else {
    Logger.log("Test passed!");
  }
}
```

`testAdd()` चलाने पर, यदि `add` फंक्शन सही तरीके से काम करता है, तो "Test passed!" लॉग होगा, या यदि यह नहीं करता है तो एक त्रुटि फेंक देगा। एक अधिक सोफ़िस्टिकेटीड दृष्टिकोण के लिए, QUnit के साथ Google Apps Script को एकीकृत करना कुछ अधिक चरणों की माँग करता है लेकिन एक शक्तिशाली परीक्षण वातावरण प्रदान करता है। एक नमूना QUnit परीक्षण सेटअप इस तरह दिखता है:

1. आपके प्रोजेक्ट में QUnit लाइब्रेरी शामिल करें।
2. QUnit परीक्षणों को चलाने के लिए एक परीक्षण HTML फाइल बनाएँ।
3. QUnit की सिंटैक्स का उपयोग करके परीक्षण केस लिखें।

यहाँ QUnit का उपयोग करके एक उदाहरण है:

```javascript
// अपने परीक्षणों को चलाने के लिए उपयोग की जाने वाली HTML फाइल में लिंक करके QUnit को शामिल करें

QUnit.test("Testing add function", function (assert) {
  var result = add(2, 3);
  assert.equal(result, 5, "add(2, 3) should return 5");
});
```

परिणाम देखने के लिए, GAS Script Editor के भीतर HTML फाइल को खोलें या इसे एक वेब ऐप के रूप में तैनात करें।

## गहरी डुबकी

ऐतिहासिक रूप से, Google Apps Script में परीक्षण को कुछ हद तक उपेक्षित रखा गया है, संभवतः मंच की उत्पत्ति और प्राथमिक उपयोग के मामलों पर ध्यान केंद्रित करने के कारण जो कि त्वरित, छोटे पैमाने के स्वचालन कार्यों पर हैं बजाय कि बड़े अनुप्रयोगों के। इस प्रकार, GAS में अधिक पारंपरिक प्रोग्रामिंग वातावरणों में पाए जाने वाले उसी रूप में सक्षम परीक्षण फ्रेमवर्क और उपकरण नहीं हैं। हालांकि, समुदाय ने ओपन-सोर्स लाइब्रेरियों को शामिल करके और गूगल के मौजूदा उपकरणों का रचनात्मक तरीके से उपयोग करके अनुकूलित किया है।

जैसे कि QUnit जैसी लाइब्रेरियों का उपयोग एक महत्वपूर्ण कदम आगे है लेकिन इसमें उचित परीक्षण वातावरण सेटअप करने और एक अतिरिक्त सिंटैक्स सीखने जैसी अपनी चुनौतियों का सेट आता है। हालाँकि, GAS के साथ अधिक जटिल और विश्वसनीय अनुप्रयोगों को बनाने में निवेश करने वालों के लिए, प्रयास इसके लायक है।

सिंपल GAS फंक्शन का उपयोग करके परीक्षण के लिए आसान उपयोग और GAS वातावरण के साथ एकीकरण प्रदान करते हैं लेकिन विस्तृत परीक्षण सुविधाओं और आपकी परियोजना के बढ़ने पर आसानी से पैमाना बनाए रखने की क्षमता का अभाव है। टूल्स जैसे कि clasp (गूगल ऐप्स स्क्रिप्ट कमांड लाइन इंटरफेस) अधिक उन्नत कार्यप्रवाहों, जिसमें परीक्षण शामिल है, को सुविधाजनक बना सकते हैं, जिससे डेवलपर्स को उनके पसंदीदा IDE में कोडिंग की अनुमति मिलती है, बाहरी परीक्षण फ्रेमवर्कों के साथ अधिक सहजता से एकीकृत करने की जगह प्रदान करती है।

निष्कर्ष में, जबकि GAS में मूल रूप से सोफ़िस्टिकेटीड परीक्षण के लिए समर्थन नहीं हो सकता है, इसकी लचीलेपन और समुदाय के नवाचारी दृष्टिकोण आपकी स्क्रिप्ट्स को दृढ़, विश्वसनीय और किसी भी कार्य के लिए तैयार बनाने के लिए व्यवहार्य मार्ग प्रदान करते हैं।
