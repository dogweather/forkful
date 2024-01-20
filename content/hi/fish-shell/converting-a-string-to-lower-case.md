---
title:                "एक स्ट्रिंग को लोअर केस में परिवर्तित करना"
html_title:           "Kotlin: एक स्ट्रिंग को लोअर केस में परिवर्तित करना"
simple_title:         "एक स्ट्रिंग को लोअर केस में परिवर्तित करना"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## क्या व क्यों?

(What & Why?)

स्ट्रिंग को लोअर केस में परिवर्तित करना ऐसा कार्य है जिसमें आपको यथासंभव सभी अक्षरों को छोटे होने का यकीन करना होता है। प्रोग्रामर्स इसे सामान्यतः टेक्स्ट तुलना जैसे कामों के लिए करते हैं, जहां मामूली केपिटलाइजेशन अंतर महत्वपूर्ण नहीं होता है।

## कैसे करें

(How to:)

ीश शेल का उपयोग करके, string को lowercase में कन्वर्ट करने के लिए आप 'string lower' कमांड का उपयोग कर सकते हैं।

उदाहरण:
```fish
string lower -a 'तुम्हारा नाम क्या है'
```

और इसका आउटपुट होगा:

```fish
'तुम्हारा नाम क्या है'
```

## गहरी बातचीत

(Deep Dive)

"string lower" आदेश आधिकारिक रूप से Fish 3.1.0 में जोड़ा गया था। अल्पसंख्यक अक्षरों को बड़े करने के विकल्प रूप में, आप `string lower -u` का उपयोग कर सकते हैं। Fish का उपयोग करने वाले डेवलपर्स इसे सुविधाजनक ढंग से स्ट्रिंग्स को मानकीकृत/समान बनाने के पब्लिक एपीआईस के रूप में पसंद करते हैं।

## देखें भी

(See Also)

अधिक जानकारी के लिए, आप Fish Shell के आधिकारिक दस्तावेज़ [यहां](https://fishshell.com/docs/current/cmds/string_lower.html) देख सकते हैं। अधिक अद्वितीय केसहेडलिंग से जुड़े कमांडों के लिए, [String Operations in Fish](https://fishshell.com/docs/current/commands.html#string) को देखें।