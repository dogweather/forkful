---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:50.524754-07:00
description: "Fish Shell \u092E\u0947\u0902 \u0928\u093F\u092F\u092E\u093F\u0924 \u0905\
  \u092D\u093F\u0935\u094D\u092F\u0915\u094D\u0924\u093F\u092F\u093E\u0901 (regex)\
  \ \u0906\u092A\u0915\u094B \u0935\u093F\u0936\u093F\u0937\u094D\u091F \u092A\u0948\
  \u091F\u0930\u094D\u0928 \u0915\u0947 \u0906\u0927\u093E\u0930 \u092A\u0930 \u0938\
  \u094D\u091F\u094D\u0930\u093F\u0902\u0917\u094D\u0938 \u0915\u094B \u0916\u094B\
  \u091C\u0928\u0947, \u092E\u093F\u0932\u093E\u0928 \u0915\u0930\u0928\u0947 \u0914\
  \u0930 \u092E\u0948\u0928\u093F\u092A\u094D\u092F\u0941\u0932\u0947\u091F \u0915\
  \u0930\u0928\u0947 \u0915\u0940 \u0905\u0928\u0941\u092E\u0924\u093F \u0926\u0947\
  \u0924\u0940 \u0939\u0948\u0902\u0964\u2026"
lastmod: '2024-03-13T22:44:53.045221-06:00'
model: gpt-4-0125-preview
summary: "Fish Shell \u092E\u0947\u0902 \u0928\u093F\u092F\u092E\u093F\u0924 \u0905\
  \u092D\u093F\u0935\u094D\u092F\u0915\u094D\u0924\u093F\u092F\u093E\u0901 (regex)\
  \ \u0906\u092A\u0915\u094B \u0935\u093F\u0936\u093F\u0937\u094D\u091F \u092A\u0948\
  \u091F\u0930\u094D\u0928 \u0915\u0947 \u0906\u0927\u093E\u0930 \u092A\u0930 \u0938\
  \u094D\u091F\u094D\u0930\u093F\u0902\u0917\u094D\u0938 \u0915\u094B \u0916\u094B\
  \u091C\u0928\u0947, \u092E\u093F\u0932\u093E\u0928 \u0915\u0930\u0928\u0947 \u0914\
  \u0930 \u092E\u0948\u0928\u093F\u092A\u094D\u092F\u0941\u0932\u0947\u091F \u0915\
  \u0930\u0928\u0947 \u0915\u0940 \u0905\u0928\u0941\u092E\u0924\u093F \u0926\u0947\
  \u0924\u0940 \u0939\u0948\u0902\u0964\u2026"
title: "\u0930\u0947\u0917\u0941\u0932\u0930 \u090F\u0915\u094D\u0938\u092A\u094D\u0930\
  \u0947\u0936\u0928\u094D\u0938 \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\
  \u0930\u0928\u093E"
---

{{< edit_this_page >}}

## क्या और क्यों?

Fish Shell में नियमित अभिव्यक्तियाँ (regex) आपको विशिष्ट पैटर्न के आधार पर स्ट्रिंग्स को खोजने, मिलान करने और मैनिप्युलेट करने की अनुमति देती हैं। प्रोग्रामर्स इनपुट वैलिडेशन, पार्सिंग, और टेक्स्ट प्रोसेसिंग जैसे कार्यों के लिए regex का उपयोग करते हैं क्योंकि यह जटिल टेक्स्ट पैटर्न्स को निर्दिष्ट करने का एक संक्षिप्त और शक्तिशाली तरीका प्रदान करता है।

## कैसे:

हालाँकि Fish Shell स्वयं में regex के लिए एक इन-बिल्ट कमांड नहीं है, यह `grep`, `sed`, और `awk` जैसे बाहरी कमांड्स का प्रभावी ढंग से उपयोग करता है जो regex का समर्थन करते हैं, जिससे आप अपनी स्क्रिप्ट्स में regex ऑपरेशन्स को जोड़ सकते हैं।

### `grep` के साथ मूल पैटर्न मिलान
किसी फाइल में लाइनों को खोजें जो एक पैटर्न से मेल खाती हैं:

```fish
grep '^[0-9]+' myfile.txt
```

यह कमांड `myfile.txt` में एक या अधिक अंकों के साथ शुरू होने वाली लाइनों को ढूँढती है।

### `sed` के साथ निकालना और बदलना
एक फाइल से फोन नंबर निकालें:

```fish
sed -n '/\([0-9]\{3\}\)-\([0-9]\{3\}\)-\([0-9]\{4\}\)/p' contacts.txt
```

`data.txt` में "foo" के सभी उदाहरणों को "bar" से बदलें:

```fish
sed 's/foo/bar/g' data.txt
```

### बुनियादी Regex के लिए `string` का उपयोग
Fish Shell की `string` कमांड साधारण regex ऑपरेशन्स जैसे कि मैच और बदलना समर्थन करती है:

एक स्ट्रिंग में एक पैटर्न मैच करें:

```fish
echo "fish 3.1.2" | string match -r '3\.[0-9]+\.[0-9]+'
```
आउटपुट:
```
3.1.2
```

'fish' के बाद आने वाले अंकों को 'X.X.X' से बदलें:

```fish
echo "Welcome to fish 3.1.2" | string replace -ra '([fish]+\s)[0-9\.]+' '$1X.X.X'
```
आउटपुट:
```
Welcome to fish X.X.X
```

### `awk` के साथ उन्नत मिलान
जहां पहला कॉलम एक विशेष पैटर्न से मेल खाता हो, वहाँ दूसरे कॉलम का डेटा प्रिंट करें:

```fish
awk '$1 ~ /^a[0-9]+$/ {print $2}' datafile
```

यह कमांड `datafile` में उन लाइनों को ढूँढता है जहां पहला कॉलम "a" से शुरू होता है जिसके बाद एक या अधिक अंक आते हैं और दूसरे कॉलम को प्रिंट करता है।

इन बाहरी कमांड्स को एकीकृत करके, Fish Shell प्रोग्रामर्स जटिल टेक्स्ट मैनिपुलेशन कार्यों के लिए नियमित अभिव्यक्तियों की पूरी शक्ति का उपयोग कर सकते हैं, शेल की मूल क्षमताओं को बढ़ाते हुए।
