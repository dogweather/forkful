---
date: 2024-01-26 00:53:49.924746-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: Fish \u092E\u0947\
  \u0902 \u090F\u0930\u0930\u094D\u0938 \u0915\u094B \u092A\u0915\u0921\u093C\u0928\
  \u0947 \u0915\u0947 \u0932\u093F\u090F, `status` \u0915\u092E\u093E\u0902\u0921\
  \ \u0914\u0930 \u0915\u0902\u0921\u0940\u0936\u0928\u0932\u094D\u0938 \u092A\u0930\
  \ \u0928\u093F\u0930\u094D\u092D\u0930 \u0930\u0939\u0947\u0902\u0964 \u092E\u093E\
  \u0928 \u0932\u0940\u091C\u093F\u090F `ping` \u0935\u093F\u092B\u0932 \u0939\u094B\
  \ \u091C\u093E\u0924\u093E \u0939\u0948; \u092F\u0939\u093E\u0901 \u0909\u0938\u0947\
  \ \u092A\u0924\u093E \u0932\u0917\u093E\u0928\u0947 \u0915\u093E \u0924\u0930\u0940\
  \u0915\u093E \u0939\u0948."
lastmod: '2024-03-13T22:44:53.079344-06:00'
model: gpt-4-1106-preview
summary: "Fish \u092E\u0947\u0902 \u090F\u0930\u0930\u094D\u0938 \u0915\u094B \u092A\
  \u0915\u0921\u093C\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F, `status` \u0915\u092E\
  \u093E\u0902\u0921 \u0914\u0930 \u0915\u0902\u0921\u0940\u0936\u0928\u0932\u094D\
  \u0938 \u092A\u0930 \u0928\u093F\u0930\u094D\u092D\u0930 \u0930\u0939\u0947\u0902\
  \u0964 \u092E\u093E\u0928 \u0932\u0940\u091C\u093F\u090F `ping` \u0935\u093F\u092B\
  \u0932 \u0939\u094B \u091C\u093E\u0924\u093E \u0939\u0948; \u092F\u0939\u093E\u0901\
  \ \u0909\u0938\u0947 \u092A\u0924\u093E \u0932\u0917\u093E\u0928\u0947 \u0915\u093E\
  \ \u0924\u0930\u0940\u0915\u093E \u0939\u0948."
title: "\u090F\u0930\u0930\u094D\u0938 \u0915\u094B \u0939\u0948\u0902\u0921\u0932\
  \ \u0915\u0930\u0928\u093E"
weight: 16
---

## कैसे करें:
Fish में एरर्स को पकड़ने के लिए, `status` कमांड और कंडीशनल्स पर निर्भर रहें। मान लीजिए `ping` विफल हो जाता है; यहाँ उसे पता लगाने का तरीका है:

```fish
ping -c 1 example.com
if not status is-success
    echo "पिंग के साथ कुछ मछली जैसा हुआ।"
end
```

अगर `ping` विफल होता है तो नमूना आउटपुट:

```
पिंग के साथ कुछ मछली जैसा हुआ।
```

एक विशिष्ट एरर कोड को संभालने के लिए, `status --is` का उपयोग करें:

```fish
false
if status --is 1
    echo "कोड 1 के साथ एक एरर पकड़ा गया।"
end
```

नमूना आउटपुट:
```
कोड 1 के साथ एक एरर पकड़ा गया।
```

एक अधिक मजबूत दृष्टिकोण के लिए, एक फंक्शन का उपयोग करने पे विचार करें:

```fish
function try_ping
    ping -c 1 example.com
    or begin
        echo "पिंग विफल हो गया स्थिति के साथ $status"
        return 1
    end
end

try_ping
```

## गहराई से समझे
Fish में एरर हैंडलिंग, उच्च-स्तरीय भाषाओं में आपको ज्ञात `try/catch` पैराडाइम से मैच नहीं करती है। इसके बजाय, आपके पास `status` कमांड द्वारा प्रदान किए गए स्पष्ट एग्ज़िट स्टेटस होते हैं।

ऐतिहासिक रूप से, Unix जैसे सिस्टमों में, एग्ज़िट स्टेटस का `0` मतलब सफलता होता है, जबकि कोई भी गैर-शून्य मूल्य एक एरर को दर्शाता है, जो आमतौर पर विभिन्न विफलता कारणों को प्रतिबिंबित करता है। यह परंपरा अधिकांश कमांड-लाइन उपयोगिताओं द्वारा और तदनुसार, स्वयं Fish द्वारा इस्तेमाल की जाती है।

Fish में `status` जांच के विकल्प में अन्य शेल्स में `trap` के माध्यम से सिग्नल हैंडलिंग शामिल है, लेकिन Fish अधिक स्पष्ट स्टेटस जांच को पसंद करता है, क्योंकि यह साफ है और कम पक्ष प्रभावों का दावा करता है।

कार्यान्वयन के दृष्टिकोण से, Fish में एरर हैंडलिंग सरल रहती है फिर भी शक्तिशाली है, ज्यादातर इसकी अवरोधन-रहित प्रकृति और स्पष्ट सिंटैक्स पर जोर देने के कारण, जैसा कि उदाहरणों में दिखाया गया है। एरर कोड्स फंक्शनों के साथ अच्छी तरह से मिश्रित होते हैं, जो मोड्यूलर और पठनीय एरर प्रबंधन के लिए अनुमति देते हैं।

## देखें भी
- Fish दस्तावेज़ पर कंडिशनल्स: https://fishshell.com/docs/current/language.html#conditionals
- Fish ट्यूटोरियल पर एरर हैंडलिंग: https://fishshell.com/docs/current/tutorial.html#error-handling
