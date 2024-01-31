---
title:                "मानक त्रुटि में लिखना"
date:                  2024-01-19
html_title:           "Arduino: मानक त्रुटि में लिखना"
simple_title:         "मानक त्रुटि में लिखना"

category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
'Standard error' (stderr) एक धारा होती है जहां प्रोग्राम त्रुटियां और लॉग संदेश दिखाता है। प्रोग्रामर डेटा और त्रुटियों को अलग रखने के लिए stderr का उपयोग करते हैं।

## How to: (कैसे करें:)
Elixir में, आप `IO` और `:stderr` एटम का उपयोग करके standard error में लिख सकते हैं:

```elixir
# साधारण त्रुटि संदेश भेज रहे हैं
IO.puts(:stderr, "त्रुटि: कुछ गलत हो गया।")

# उपयोगकर्ता-डिफ़ाइंड लॉग फॉर्मेटर के साथ त्रुटि संदेश
IO.write(:stderr, "[ERROR] #{DateTime.utc_now()}: फाइल नहीं खुल सकी।\n")
```

उपरोक्त कोड जब चलाया जाएगा तो आपको निम्नलिखित आउटपुट नजर आएगा:

```
त्रुटि: कुछ गलत हो गया।
[ERROR] 2023-04-01T00:00:00Z: फाइल नहीं खुल सकी।
```

## Deep Dive (गहराई से समझें):
Standard output (stdout) और standard error (stderr) की अवधारणा Unix परंपरा से आई है, जहां stdout को फ़ाइल डिस्क्रिप्टर 1 और stderr को फ़ाइल डिस्क्रिप्टर 2 दिया गया है। Elixir, जो Erlang VM पर चलता है, इन विचारों को इस्तेमाल करता है और IO मॉड्यूल के जरिए इंटरफ़ेस प्रदान करता है। अल्टरनेटिव के तौर पर, कुछ लाइब्रेरीज जैसे `Logger` भी उपलब्ध हैं जो मोर डिटेल्ड और फ्लेक्सिबल लोगिंग सोलूशंस प्रोवाइड करती हैं, खासकर एप्लिकेशन लेवल पर।

## See Also (और जानकारी के सूत्र):
- [Elixir IO Module Documentation](https://hexdocs.pm/elixir/IO.html)
- [Erlang's :io module](http://erlang.org/doc/man/io.html)
- [The Logger Elixir Library](https://hexdocs.pm/logger/Logger.html)
