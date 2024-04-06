---
date: 2024-01-20 17:39:41.522739-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902) Bash \u092E\
  \u0947\u0902 \u0906\u092A \u0928\u093F\u092E\u094D\u0928\u0932\u093F\u0916\u093F\
  \u0924 \u0915\u092E\u093E\u0902\u0921\u094D\u0938 \u0915\u093E \u092A\u094D\u0930\
  \u092F\u094B\u0917 \u0915\u0930\u0915\u0947 \u0924\u093E\u0924\u094D\u0915\u093E\
  \u0932\u093F\u0915 \u092B\u093E\u0907\u0932 \u0924\u0948\u092F\u093E\u0930 \u0915\
  \u0930 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902."
lastmod: '2024-04-05T21:53:54.629301-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902) Bash \u092E\u0947\u0902\
  \ \u0906\u092A \u0928\u093F\u092E\u094D\u0928\u0932\u093F\u0916\u093F\u0924 \u0915\
  \u092E\u093E\u0902\u0921\u094D\u0938 \u0915\u093E \u092A\u094D\u0930\u092F\u094B\
  \u0917 \u0915\u0930\u0915\u0947 \u0924\u093E\u0924\u094D\u0915\u093E\u0932\u093F\
  \u0915 \u092B\u093E\u0907\u0932 \u0924\u0948\u092F\u093E\u0930 \u0915\u0930 \u0938\
  \u0915\u0924\u0947 \u0939\u0948\u0902."
title: "\u0905\u0938\u094D\u0925\u093E\u092F\u0940 \u092B\u093E\u0907\u0932 \u092C\
  \u0928\u093E\u0928\u093E"
weight: 21
---

## How to: (कैसे करें)
Bash में आप निम्नलिखित कमांड्स का प्रयोग करके तात्कालिक फाइल तैयार कर सकते हैं:

```Bash
# tmpfile पर अस्थायी फाइल बनाएं
tmpfile=$(mktemp)

# tmpfile में कुछ डेटा जोड़ें
echo "यह कुछ अस्थायी डेटा है" > "$tmpfile"

# tmpfile को दिखाएं
cat "$tmpfile"

# tmpfile को हटा दें
rm "$tmpfile"
```

सैंपल आउटपुट:

```Bash
यह कुछ अस्थायी डेटा है
```

## Deep Dive (गहन जानकारी)
तात्कालिक फाइलें यूनिक्स-लाइक सिस्टम्स में `/tmp` डायरेक्टरी में बनाई जाती हैं। `mktemp` कमांड 2003 से उपलब्ध है और यह यूनिक फाइलनाम बनाती है जिससे डेटा ओवरलैप की समस्या नहीं होती। वैकल्पिक रूप से, `mktemp -d` का प्रयोग करके अस्थायी डायरेक्टरी भी तैयार की जा सकती है। 

कोड में `$$` (प्रोसेस आईडी) जोड़कर पुराने शैलियों में भी अस्थायी फाइलें बनाई जा सकती थीं, पर यह तरीका सुरक्षित नहीं है क्योंकि यह संघर्ष (collision) की संभावना को बढ़ाता है।

## See Also (अधिक जानकारी के लिए)
- GNU Coreutils `mktemp` Manual: [https://www.gnu.org/software/coreutils/manual/html_node/mktemp-invocation.html](https://www.gnu.org/software/coreutils/manual/html_node/mktemp-invocation.html)
- Advanced Bash-Scripting Guide: [https://tldp.org/LDP/abs/html/](https://tldp.org/LDP/abs/html/)
