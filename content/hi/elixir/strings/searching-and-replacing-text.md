---
title:                "पाठ खोजना और बदलना"
aliases:
- /hi/elixir/searching-and-replacing-text/
date:                  2024-01-20T17:57:41.920697-07:00
model:                 gpt-4-1106-preview
simple_title:         "पाठ खोजना और बदलना"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
टेक्स्ट खोजना और बदलना मतलब होता है पूरे टेक्स्ट में से कोई शब्द या फ्रेज़ को ढूंढ कर उसको किसी दूसरे शब्द या फ्रेज़ से बदल देना। प्रोग्रामर्स इसे डाटा को संशोधित करने, गलतियों को सही करने, या कोड को अपडेट करने के लिए करते हैं।

## How to: (कैसे करें)
Elixir में टेक्स्ट खोजने और बदलने के लिए `String.replace/4` फंक्शन का उपयोग करें:

```elixir
# सिंपल टेक्स्ट रिप्लेसमेंट
original_text = "हेलो, मेरा नाम अनिल है।"
new_text = String.replace(original_text, "अनिल", "राहुल")
IO.puts new_text
# Output: हेलो, मेरा नाम राहुल है।

# रेगुलर एक्सप्रेशन का इस्तेमाल करते हुए टेक्स रिप्लेसमेंट
regex = ~r/नाम \w+/
updated_text = String.replace(original_text, regex, "नाम सुधीर")
IO.puts updated_text
# Output: हेलो, मेरा नाम सुधीर है।
```

## Deep Dive (गहराई से समझें)
एलिक्सिर में टेक्स्ट खोजना और बदलना Erlang VM पर बनाया गया है, जो इसे बहुत तेज़ और सक्षम बनाता है। इतिहास से देखें तो, टेक्स्ट मैनिपुलेशन शुरुआती प्रोग्रामिंग टास्क्स में से एक है। शुरुआत में Unix टेक्स्ट एडिटर्स जैसे 'sed' और 'awk' इसके लिए प्रयोग किये जाते थे। एलिक्सिर में स्ट्रिंग लाइब्रेरी इस कर्म को आसान बनाती है और `String.replace/4` फंक्शन परफॉर्मेंस और फ्लेक्सिबिलिटी का शानदार मिश्रण है। इसके अल्टरनेटिव्स में पाइपलाइन ऑपरेटर्स का इस्तेमाल कर के चेन रिप्लेसमेंट्स और पैटर्न मैचिंग शामिल है।

## See Also (और जानकारी)
- [Elixir स्ट्रिंग डॉक्स](https://hexdocs.pm/elixir/String.html)
- [Regex गाइड](https://hexdocs.pm/elixir/Regex.html)
