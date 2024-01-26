---
title:                "यादृच्छिक संख्याएँ उत्पन्न करना"
date:                  2024-01-20T17:49:35.180396-07:00
model:                 gpt-4-1106-preview
simple_title:         "यादृच्छिक संख्याएँ उत्पन्न करना"
programming_language: "Bash"
category:             "Bash"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
रैंडम नंबर जेनरेशन, यानी बेतरतीब संख्याओं का निर्माण, वह प्रक्रिया है जिसके माध्यम से हम एक या एक से अधिक संख्यायें प्राप्त करते हैं जो किसी विशिष्ट पैटर्न का पालन नहीं करती हैं। प्रोग्रामर्स यह काम डेटा एनालिसिस, गेम डेवलपमेंट, सिक्योरिटी और टेस्टिंग स्क्रिप्ट्स में नॉन-प्रेडिक्टेबल एलेमेंट्स लाने के लिए करते हैं।

## How to: (कैसे करें:)
```Bash
# एक रैंडम नंबर जेनरेट करने के लिए
echo $(( RANDOM ))

# एक निश्चित रेंज में रैंडम नंबर (1-100) जेनरेट करने के लिए
echo $(( 1 + RANDOM % 100 ))

# एक सक्रियता प्राप्त करने के लिए - उदाहरण स्वरूप "dice" रोल करना
echo $(( 1 + RANDOM % 6 ))
```
पहला कमांड आपको 0 से 32767 तक कोई भी रैंडम नंबर देगा। दूसरे कमांड से आप 1 से 100 के बीच में कोई भी नंबर प्राप्त कर सकते हैं। तीसरे कमांड की मदद से "डाइस" चला सकते हैं।

## Deep Dive (गहराई से जानकारी)
रैंडम नंबर्स की जरुरत पुराने कंप्यूटर के जमाने से है, जब लोग गणितीय सिमुलेशंस और सांख्यिकीय विश्लेषण में इनका उपयोग करते थे। बैश में रैंडम नंबर जेनरेट करने के लिए `$RANDOM` एक बिल्ट-इन वैरिएबल है, लेकिन यह एक पूर्णतया यादृच्छिक (true random) परिणाम नहीं देता है। यह प्रयोग के लिए ठीक है लेकिन सिक्योरिटी-संवेदनशील प्रोग्राम्स के लिए नहीं। वहां आपको `/dev/random` या `/dev/urandom` का उपयोग करने की जरूरत हो सकती है। `$RANDOM` एक पीएसईयूडो-रैंडम (pseudorandom) जेनरेटर है जो कि एक औपचारिक शुरुआती बिंदु (seed) से संख्याएं उत्पन्न करता है। इसका मतलब है कि अगर आप सीड को जान जाएं, तो संख्या श्रृंखला को दोबारा उत्पन्न किया जा सकता है।

## See Also (और भी जानकारी)
- Bash मैनुअल पेज: [Bash Reference Manual](https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html)
- लिनक्स डाक्युमेंटेशन प्रोजेक्ट: [Advanced Bash-Scripting Guide](http://tldp.org/LDP/abs/html/)
- Stack Overflow: [Using RANDOM in Bash](https://stackoverflow.com/questions/2556190/random-number-from-a-range-in-a-bash-script)
- `/dev/random` और `/dev/urandom` समझने के लिए: [Understanding /dev/random](https://www.2uo.de/myths-about-urandom/)
