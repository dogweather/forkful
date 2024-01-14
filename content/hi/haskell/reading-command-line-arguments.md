---
title:    "Haskell: कम्प्यूटर प्रोग्रामिंग पर लेख: कमांड लाइन आर्ग्यूमेंट्स पढ़ना"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## क्यों

कमांड लाइन आर्ग्यूमेंट पढ़ने का क्या फायदा हो सकता है? कमांड लाइन प्रोग्रामिंग आपको अपने कोड को सीधे एक्सेस करने और तेजी से परिवर्तन करने की सुविधा प्रदान करती है। कमांड लाइन आर्ग्यूमेंट अपने प्रोग्राम को पारामीटर के रूप में स्वीकार करने और उनसे फ्लैग के रूप में प्रभावी ढंग से इस्तेमाल करने की अनुमति देते हैं। यह आपको अपने प्रोग्राम को अधिक एकीकृत बनाने में मदद कर सकता है और उच्च स्तर के प्रोग्रामिंग अनुभव प्रदान कर सकता है।

## कैसे

इस हास्केल ब्लोग पोस्ट में, हम कमांड लाइन आर्ग्यूमेंट पढ़ने के विभिन्न तरीकों को समझेंगे। आर्ग्यूमेंट को आरंभ और अंत तोड़नेवाले एक साधारण उदाहरण के साथ "getArgs" फ़ंक्शन का उपयोग करके प्रिंट करना। इसके बाद, हम आर्ग्यूमेंट को विशिष्ट डेटा टाइप में परिवर्तन करने के लिए ऱेंग्क (pattern matching) को उपयोग करेंगे। अंत में, आर्ग्यूमेंट को "optparse-applicative" पैकेज का उपयोग करके अधिक प्रभावी ढंग से हैंडल करेंगे।

```Haskell
import System.Environment
import Options.Applicative

-- उदाहरण प्रिंट करने के लिए getArgs फ़ंक्शन
main = do
  args <- getArgs
  print "आपने ये आर्ग्यूमेंट पास किए:"
  print args

-- पैटर्न मैचिंग का उपयोग करने के लिए आर्ग्यूमेंट के लिए एक डेटा टाइप स्पष्ट करें
main = do
  (name:age:_) <- get