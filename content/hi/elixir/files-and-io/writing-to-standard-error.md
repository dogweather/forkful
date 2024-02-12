---
title:                "मानक त्रुटि के लिए लिखना"
date:                  2024-02-03T19:34:25.989907-07:00
model:                 gpt-4-0125-preview
simple_title:         "मानक त्रुटि के लिए लिखना"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

एलिक्सिर में स्टैंडर्ड एरर (stderr) में लिखना मुख्य आउटपुट (stdout) से अलग त्रुटि संदेशों और निदानों को निर्दिष्ट करने की एक विधि है। प्रोग्रामर मुख्य आउटपुट को अव्यवस्थित किए बिना त्रुटियों को डिबग और संभालने के लिए स्टेंडर्ड एरर का उपयोग करते हैं, जिससे समस्याओं की पहचान और संबोधन करना आसान हो जाता है।

## कैसे करें:

एलिक्सिर में, आप `IO` मॉड्यूल फंक्शन्स जैसे कि `IO.puts/2` और `IO.warn/2` का उपयोग करके स्टैंडर्ड एरर में संदेश लिख सकते हैं:

```elixir
# stderr में एक साधारण संदेश लिखना
IO.puts(:stderr, "गलती: कुछ गलत हो गया!")

# त्रुटियों/चेतावनियों के लिए अधिक सार्थक IO.warn का उपयोग करना
IO.warn("चेतावनी: आप सीमा को पार करने वाले हैं!")
```

`IO.puts/2` के लिए टर्मिनल में नमूना आउटपुट:
```
गलती: कुछ गलत हो गया!
```

`IO.warn/2` के लिए, आउटपुट समान होगा, लेकिन `IO.warn/2` विशेष रूप से चेतावनियों के लिए डिज़ाइन किया गया है और भविष्य के एलिक्सिर संस्करणों में अतिरिक्त फॉर्मेटिंग या व्यवहार शामिल कर सकता है।

**तृतीय-पक्ष पुस्तकालयों का उपयोग करना**

जबकि एलिक्सिर की मानक पुस्तकालय सामान्यतः स्टैंडर्ड एरर आउटपुट को संभालने के लिए पर्याप्त होती है, आप अधिक जटिल अनुप्रयोगों के लिए या विभिन्न लॉग स्तरों और आउटपुट्स को कॉन्फिगर करने के लिए `Logger` जैसी पुस्तकालयों को उपयोगी पाएंगे।

एक त्रुटि संदेश आउटपुट करने के लिए `Logger` का उपयोग करने का उदाहरण:

```elixir
require Logger

# Logger को stderr में आउटपुट करने के लिए कॉन्फिगर करें
Logger.configure_backend(:console, device: :stderr)

# एक त्रुटि संदेश लिखना
Logger.error("गलती: डेटाबेस से कनेक्ट करने में विफल।")
```

यह सेटअप `Logger` का आउटपुट विशेष रूप से स्टैंडर्ड एरर की ओर निर्देशित करता है, जो स्टैंडर्ड लॉग संदेशों से त्रुटि लॉगिंग को अलग करने के लिए उपयोगी है।