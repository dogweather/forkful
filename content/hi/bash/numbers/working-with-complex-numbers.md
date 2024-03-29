---
date: 2024-01-26 04:37:54.888130-07:00
description: "\u091C\u091F\u093F\u0932 \u0938\u0902\u0916\u094D\u092F\u093E\u090F\u0901\
  \ \u090F\u0915 \u0935\u093E\u0938\u094D\u0924\u0935\u093F\u0915 \u092D\u093E\u0917\
  \ \u0914\u0930 \u090F\u0915 \u0915\u093E\u0932\u094D\u092A\u0928\u093F\u0915 \u092D\
  \u093E\u0917 \u0938\u0947 \u092E\u093F\u0932\u0915\u0930 \u092C\u0928\u0940 \u0939\
  \u094B\u0924\u0940 \u0939\u0948\u0902\u0964 \u092A\u094D\u0930\u094B\u0917\u094D\
  \u0930\u093E\u092E\u0930 \u0938\u093F\u0917\u094D\u0928\u0932 \u092A\u094D\u0930\
  \u094B\u0938\u0947\u0938\u093F\u0902\u0917, \u0915\u094D\u0935\u093E\u0902\u091F\
  \u092E \u092F\u093E\u0902\u0924\u094D\u0930\u093F\u0915\u0940 \u0914\u0930 \u091C\
  \u092C \u092D\u0940 \u0917\u0923\u0928\u093E \u0915\u0940 \u0906\u0935\u0936\u094D\
  \u092F\u0915\u0924\u093E \u0939\u094B\u0924\u0940 \u0939\u0948,\u2026"
lastmod: '2024-03-13T22:44:52.612066-06:00'
model: gpt-4-0125-preview
summary: "\u091C\u091F\u093F\u0932 \u0938\u0902\u0916\u094D\u092F\u093E\u090F\u0901\
  \ \u090F\u0915 \u0935\u093E\u0938\u094D\u0924\u0935\u093F\u0915 \u092D\u093E\u0917\
  \ \u0914\u0930 \u090F\u0915 \u0915\u093E\u0932\u094D\u092A\u0928\u093F\u0915 \u092D\
  \u093E\u0917 \u0938\u0947 \u092E\u093F\u0932\u0915\u0930 \u092C\u0928\u0940 \u0939\
  \u094B\u0924\u0940 \u0939\u0948\u0902\u0964 \u092A\u094D\u0930\u094B\u0917\u094D\
  \u0930\u093E\u092E\u0930 \u0938\u093F\u0917\u094D\u0928\u0932 \u092A\u094D\u0930\
  \u094B\u0938\u0947\u0938\u093F\u0902\u0917, \u0915\u094D\u0935\u093E\u0902\u091F\
  \u092E \u092F\u093E\u0902\u0924\u094D\u0930\u093F\u0915\u0940 \u0914\u0930 \u091C\
  \u092C \u092D\u0940 \u0917\u0923\u0928\u093E \u0915\u0940 \u0906\u0935\u0936\u094D\
  \u092F\u0915\u0924\u093E \u0939\u094B\u0924\u0940 \u0939\u0948,\u2026"
title: "\u091C\u091F\u093F\u0932 \u0938\u0902\u0916\u094D\u092F\u093E\u0913\u0902\
  \ \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\u093E"
---

{{< edit_this_page >}}

## क्या और क्यों?
जटिल संख्याएँ एक वास्तविक भाग और एक काल्पनिक भाग से मिलकर बनी होती हैं। प्रोग्रामर सिग्नल प्रोसेसिंग, क्वांटम यांत्रिकी और जब भी गणना की आवश्यकता होती है, इनका उपयोग करते हैं, क्योंकि सामान्य वास्तविक संख्याएँ बस काफी नहीं होतीं।

## कैसे करें:
Bash मूल रूप से जटिल संख्याओं का समर्थन नहीं करता। आप अक्सर `-l` विकल्प के साथ `bc` जैसे बाहरी उपकरण का उपयोग करेंगे। यहाँ बैश में जटिल संख्याओं को कैसे संसाधित किया जाता है:

```bash
echo "sqrt(-1)" | bc -l
```

आउटपुट:
```bash
j
```

गुणा:

```bash
echo "(-1 + -1i) * (4 + 3i)" | bc -l
```

आउटपुट:
```bash
-1.00000000000000000000-7.00000000000000000000i
```

## गहराई से समझना
जटिल संख्याएँ 16वीं शताब्दी से रही हैं, लेकिन जैसे बैश जैसी लिपि भाषाएँ जटिल संख्याओं जैसे गणितीय गणनाओं के लिए सीधे तैयार नहीं होती हैं। यही कारण है कि `bc` या `awk` जैसे अन्य उपकरण अक्सर काम में आते हैं। जटिल संख्याओं के साथ काम करने के लिए कुछ विकल्प भाषाएँ Python अपने `cmath` मॉड्यूल के साथ और MATLAB हैं, जो दोनों अधिक उन्नत गणितीय कार्यों के लिए निर्मित हैं। बैश के लिए, यह सब उपकरणों का लाभ उठाने के बारे में है - `bc` काल्पनिक इकाई को दर्शाने के लिए लोअरकेस 'i' का उपयोग करता है और यह जोड़, घटाव, गुणा, और भाग जैसे मूल कार्यों का समर्थन करता है।

## यह भी देखें
- `bc` मैनुअल: https://www.gnu.org/software/bc/manual/html_mono/bc.html
- GNU Octave (MATLAB के लिए विकल्प): https://www.gnu.org/software/octave/
- Python `cmath` मॉड्यूल: https://docs.python.org/3/library/cmath.html
