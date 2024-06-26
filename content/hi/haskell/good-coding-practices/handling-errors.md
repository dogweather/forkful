---
date: 2024-01-26 00:55:36.802706-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: Haskell `Maybe` \u0914\
  \u0930 `Either` \u091C\u0948\u0938\u0947 \u091F\u093E\u0907\u092A\u094B\u0902 \u0915\
  \u0947 \u091C\u0930\u093F\u090F \u092E\u091C\u092C\u0942\u0924\u0940 \u0938\u0947\
  \ \u0924\u094D\u0930\u0941\u091F\u093F\u092F\u094B\u0902 \u0915\u094B \u0938\u0902\
  \u092D\u093E\u0932\u0924\u093E \u0939\u0948\u0964 \u092F\u0939\u093E\u0901 \u090F\
  \u0915 \u0924\u094D\u0935\u0930\u093F\u0924 \u0928\u091C\u093C\u0930."
lastmod: '2024-03-13T22:44:52.417686-06:00'
model: gpt-4-1106-preview
summary: "Haskell `Maybe` \u0914\u0930 `Either` \u091C\u0948\u0938\u0947 \u091F\u093E\
  \u0907\u092A\u094B\u0902 \u0915\u0947 \u091C\u0930\u093F\u090F \u092E\u091C\u092C\
  \u0942\u0924\u0940 \u0938\u0947 \u0924\u094D\u0930\u0941\u091F\u093F\u092F\u094B\
  \u0902 \u0915\u094B \u0938\u0902\u092D\u093E\u0932\u0924\u093E \u0939\u0948\u0964\
  \ \u092F\u0939\u093E\u0901 \u090F\u0915 \u0924\u094D\u0935\u0930\u093F\u0924 \u0928\
  \u091C\u093C\u0930."
title: "\u090F\u0930\u0930\u094D\u0938 \u0915\u094B \u0939\u0948\u0902\u0921\u0932\
  \ \u0915\u0930\u0928\u093E"
weight: 16
---

## कैसे करें:
Haskell `Maybe` और `Either` जैसे टाइपों के जरिए मजबूती से त्रुटियों को संभालता है। यहाँ एक त्वरित नज़र:

```Haskell
safeDivide :: Integral a => a -> a -> Maybe a
safeDivide _ 0 = Nothing  -- शून्य से भाग देना नहीं होता, इसलिए हम Nothing लौटाते हैं।
safeDivide x y = Just (x `div` y)  -- अन्यथा, हम सब ठीक हैं, परिणाम Just में लौटाएं।

-- इसे कार्रवाई में देखें:
example1 :: Maybe Int
example1 = safeDivide 10 2  -- Just 5

example2 :: Maybe Int
example2 = safeDivide 10 0  -- Nothing
```

अधिक जटिल त्रुटि संभालने के लिए, `Either` का उपयोग होता है:

```Haskell
safeDivideEither :: Integral a => a -> a -> Either String a
safeDivideEither _ 0 = Left "Divide by zero error."  -- इस बार, त्रुटि एक सन्देश के साथ आती है।
safeDivideEither x y = Right (x `div` y)

-- और उपयोग में:
example3 :: Either String Int
example3 = safeDivideEither 10 2  -- Right 5

example4 :: Either String Int
example4 = safeDivideEither 10 0  -- Left "Divide by zero error."
```

## गहराई से समझें
Haskell की दुनिया में, त्रुटि संभालने का एक मजबूत इतिहास रहा है। पुराने दिनों में, त्रुटियाँ आपके पूरे प्रोग्राम को गिरा सकती थीं - मजेदार नहीं। Haskell की टाइप सिस्टम इसे बहुत कम संभावना वाला बनाने के तरीके प्रदान करती है। हमारे पास `Maybe` और `Either` हैं, लेकिन अलग-अलग परिस्थितियों के लिए `Exceptions` और `IO` जैसे अन्य भी हैं।

`Maybe` सरल है: अगर सब ठीक है तो आपको `Just` कुछ मिलता है, या अगर नहीं है तो `Nothing` मिलता है। `Either` इसे और आगे बढ़ाता है, जिससे आप को एक त्रुटि सन्देश (`Left`) या एक सफल परिणाम (`Right`) वापस करने की अनुमति मिलती है।

दोनों प्योर हैं, यानी वे बाहर की दुनिया के साथ छेड़छाड़ नहीं करते - Haskell में यह एक बड़ी बात है। हम अनियंत्रित अपवादों की उन समस्याओं से बचते हैं जो कुछ अन्य भाषाओं में व्याप्त हैं।

उन लोगों के लिए जो `Maybe` और `Either` से संतुष्ट नहीं हैं, `Control.Exception` जैसी लाइब्रेरीज अपवादों के माध्यम से अधिक पारंपरिक, आज्ञाकारी-शैली की त्रुटि संभालने प्रदान करती हैं। लेकिन उन्हें अत्यधिक स्वतंत्रतापूर्वक उपयोग करने से चीजें जटिल हो सकती हैं, इसलिए समुदाय अक्सर टाइपों पर टिका रहता है।

## देखें भी
और गहराई से जानें:

- Haskell के अपने दस्तावेज: [Haskell](https://haskell.org/documentation)
- शुरुआती के लिए उत्तम: ["Learn You a Haskell for Great Good!"](http://learnyouahaskell.com/)
