---
title:                "अस्थायी फाइल बनाना"
date:                  2024-01-20T17:39:41.522739-07:00
model:                 gpt-4-1106-preview
simple_title:         "अस्थायी फाइल बनाना"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/bash/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)

तात्कालिक फाइल बनाना एक ऐसी प्रक्रिया है जहाँ हम एक अस्थायी फाइल तैयार करते हैं, डेटा रखते हैं, और फिर उसे हटा देते हैं। यह डेटा सुरक्षा, परीक्षण और स्पेस मैनेजमेंट के लिए महत्वपूर्ण होता है।

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
