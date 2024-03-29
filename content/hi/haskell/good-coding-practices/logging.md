---
date: 2024-01-26 01:09:41.165441-07:00
description: "\u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u093F\u0902\u0917\
  \ \u092E\u0947\u0902 \u0932\u0949\u0917\u093F\u0902\u0917 \u0915\u093E \u092E\u0942\
  \u0932\u0924\u0903 \u0905\u0930\u094D\u0925 \u0939\u094B\u0924\u093E \u0939\u0948\
  \ \u0918\u091F\u0928\u093E\u0913\u0902 \u092F\u093E \u0938\u0902\u0926\u0947\u0936\
  \u094B\u0902 \u0915\u093E \u090F\u0915 \u0930\u0947\u0915\u0949\u0930\u094D\u0921\
  \ \u092C\u0928\u093E\u0915\u0930 \u0909\u0928\u0915\u093E \u0928\u093F\u0936\u093E\
  \u0928 \u091B\u094B\u0921\u093C \u0926\u0947\u0928\u093E, \u091C\u093F\u0938\u0947\
  \ \u092F\u0939 \u091F\u094D\u0930\u0948\u0915 \u0915\u0930\u0928\u0947 \u0915\u0947\
  \ \u0932\u093F\u090F \u0909\u092A\u092F\u094B\u0917 \u0915\u093F\u092F\u093E \u091C\
  \u093E \u0938\u0915\u0924\u093E \u0939\u0948\u2026"
lastmod: '2024-03-13T22:44:52.415944-06:00'
model: gpt-4-1106-preview
summary: "\u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u093F\u0902\u0917\
  \ \u092E\u0947\u0902 \u0932\u0949\u0917\u093F\u0902\u0917 \u0915\u093E \u092E\u0942\
  \u0932\u0924\u0903 \u0905\u0930\u094D\u0925 \u0939\u094B\u0924\u093E \u0939\u0948\
  \ \u0918\u091F\u0928\u093E\u0913\u0902 \u092F\u093E \u0938\u0902\u0926\u0947\u0936\
  \u094B\u0902 \u0915\u093E \u090F\u0915 \u0930\u0947\u0915\u0949\u0930\u094D\u0921\
  \ \u092C\u0928\u093E\u0915\u0930 \u0909\u0928\u0915\u093E \u0928\u093F\u0936\u093E\
  \u0928 \u091B\u094B\u0921\u093C \u0926\u0947\u0928\u093E, \u091C\u093F\u0938\u0947\
  \ \u092F\u0939 \u091F\u094D\u0930\u0948\u0915 \u0915\u0930\u0928\u0947 \u0915\u0947\
  \ \u0932\u093F\u090F \u0909\u092A\u092F\u094B\u0917 \u0915\u093F\u092F\u093E \u091C\
  \u093E \u0938\u0915\u0924\u093E \u0939\u0948\u2026"
title: "\u0932\u0949\u0917\u093F\u0902\u0917"
---

{{< edit_this_page >}}

## क्या और क्यों?
प्रोग्रामिंग में लॉगिंग का मूलतः अर्थ होता है घटनाओं या संदेशों का एक रेकॉर्ड बनाकर उनका निशान छोड़ देना, जिसे यह ट्रैक करने के लिए उपयोग किया जा सकता है कि आपका एप्लीकेशन किसी भी दिए गए क्षण में क्या कर रहा है। प्रोग्रामर्स इसे इशूज को डिबग करने, सिस्टम परफ़ॉर्मेंस को मोनिटर करने, और सुरक्षा तथा अनुपालन कारणों के लिए व्यवहार को ऑडिट करने के लिए करते हैं।

## कैसे करें:
हास्केल में, लॉगिंग `monad-logger` या `hslogger` जैसे पुस्तकालयों का उपयोग करके लागू किया जा सकता है। यहाँ `monad-logger` का उपयोग करते हुए एक त्वरित उदाहरण दिया गया है:

```Haskell
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Logger
import Control.Monad.IO.Class (liftIO)

logExample :: LoggingT IO ()
logExample = do
    logInfoN "एप्लीकेशन शुरू करते हुए..."
    liftIO $ putStrLn "कुछ महत्वपूर्ण काम कर रहे हैं..."
    logErrorN "उह! कुछ गलत हो गया।"

main :: IO ()
main = runStdoutLoggingT logExample

{- सैंपल आउटपुट
[Info] एप्लीकेशन शुरू करते हुए...
कुछ महत्वपूर्ण काम कर रहे हैं...
[Error] उह! कुछ गलत हो गया।
-}
```

यह सरल उदाहरण बताता है कि कैसे आप समझने के लिए कि रनटाइम पर क्या हो रहा है, आपके कोड में लॉगिंग स्टेटमेंट्स को बिखेर सकते हैं। `logInfoN` और `logErrorN` का उपयोग क्रमशः सूचनात्मक और त्रुटि संदेशों को लॉग करने के लिए किया जाता है।

## गहराई में:
लॉगिंग सामान्य प्रिंट स्टेटमेंट्स से लेकर सोप्सिस्टिकेटेड लॉगिंग फ्रेमवर्क्स तक एक लंबी यात्रा तय कर चुकी है। ऐतिहासिक रूप से, लॉग्स सिर्फ कंसोल या फाइलों में टेक्स्ट आउटपुट्स होते थे, पर अब वे संरचित डेटा शामिल करते हैं जिन्हें विभिन्न उपकरणों द्वारा पार्स और विश्लेषण किया जा सकता है।

हास्केल में, लॉगिंग को शुद्ध कार्यात्मक शैली में जो स्पष्ट रूप से लॉग क्रियाओं को प्रसारित करती है, या अशुद्धता के लिए मोनादिक संदर्भों का उपयोग करके किया जा सकता है, जहाँ लॉगर्स गणना के माध्यम से अंतर्निहित रूप से थ्रेडेड होते हैं।

उदाहरण के लिए, `hslogger` लाइब्रेरी `monad-logger` की तुलना में अधिक पारंपरिक और परिवर्तनशील है। `monad-logger` मोनाद स्टैक के साथ इंटीग्रेशन प्रदान करता है और आउटपुट फॉर्मेटिंग और नियंत्रण के मामले में अधिक लचीलापन प्रदान करता है। दोनों लाइब्रेरीज आपको लॉग स्तर सेट करने की अनुमति देती हैं, जो उनके महत्व के आधार पर लॉग संदेशों को छानने में मदद करती हैं। लॉग स्तर में डीबग, इन्फो, नोटिस, वार्निंग, एरर, क्रिटिकल, अलर्ट, और इमर्जेंसी शामिल हैं।

हास्केल की लॉगिंग की दृष्टिकोण अक्सर उसके टाइप सेफ्टी और शुद्धिकरण पर जोर के साथ तालमेल रखती है। लॉग्स को इस तरह से संभाला जा सकता है कि यदि लॉगिंग विफल हो जाती है, तो यह हास्केल के मजबूत त्रुटि संभालने की क्षमताओं के कारण मुख्य एप्लीकेशन को क्रैश करने का कारण नहीं बनेगा।

## देखें भी:

- [`monad-logger` हैकेज पर डॉक्यूमेंटेशन](https://hackage.haskell.org/package/monad-logger)
- [`hslogger` पैकेज हैकेज पर](https://hackage.haskell.org/package/hslogger)
- [रीयल वर्ल्ड हास्केल, चैप्टर 19, त्रुटि संभालने वाले पर](http://book.realworldhaskell.org/read/error-handling.html)
- [हास्केल के लिए लॉगिंग फेसेड (log-base)](https://hackage.haskell.org/package/log-base)
