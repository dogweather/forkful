---
date: 2024-01-20 17:36:02.432497-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:)."
lastmod: '2024-04-05T21:53:54.616918-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u0924\u093E\u0930\u0940\u0916 \u0915\u094B \u0938\u094D\u091F\u094D\u0930\
  \u093F\u0902\u0917 \u092E\u0947\u0902 \u092C\u0926\u0932\u0928\u093E"
weight: 28
---

## How to:
(कैसे करें:)

```Bash
# वर्तमान तारीख और समय प्राप्त करें
current_date=$(date)
echo "वर्तमान दिनांक: $current_date"

# तारीख को निर्धारित फॉरमैट में दिखाएँ
formatted_date=$(date +"%d-%m-%Y")
echo "फॉर्मेटेड दिनांक: $formatted_date"
```

सैंपल आउटपुट:

```
वर्तमान दिनांक: Wed Mar 10 10:26:35 PST 2023
फॉर्मेटेड दिनांक: 10-03-2023
```

## Deep Dive:
(गहराई से जानकारी:)

Bash में `date` कमांड लंबे समय से यूनिक्स-आधारित सिस्टम्स पर तारीख और समय संभालने के लिए मौजूद है। स्ट्रिंग फॉर्मेटिंग के लिए `%d`, `%m`, `%Y` जैसे प्लेसहोल्डर्स इस्तेमाल किए जाते हैं। अल्टरनेटिव के रूप में, `awk`, `sed`, या `perl` स्क्रिप्टिंग को डेटा प्रोसेसिंग पाइपलाइन में जोड़ा जा सकता है। लेकिन, `date` कमांड अधिक सरल है और डायरेक्ट तारीखों को हैंडल करता है।

## See Also:
(और भी देखें:)

- GNU Coreutils: https://www.gnu.org/software/coreutils/manual/coreutils.html#date-invocation
- Advanced Bash-Scripting Guide: https://tldp.org/LDP/abs/html/datesandtimes.html
- Bash Reference Manual: https://www.gnu.org/software/bash/manual/bash.html#Shell-Parameters
