---
title:    "Elixir: डिबग आउटपुट प्रिंट करना"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## क्यों

Debug output को प्रिंट करना Elixir programming में एक बहुत ही महत्वपूर्ण क्रिया है। यह एक प्रोग्रामर को त्रुटियों को मान्यता देने में और प्रोग्रामिंग लॉजिक को समझने में मदद करता है। इससे दोनों टेस्टिंग और डिबगिंग प्रक्रियाओं में समय और श्रम बचाया जा सकता है।

## कैसे

Debug output प्रिंट करने के लिए, आपको `IO.inspect/2` फंक्शन का उपयोग करना होगा। यह फंक्शन दो arguments लेता है - प्रिंट करने के लिए variable और debug message।

इसके अलावा, आप `IO.inspect/2` के डिफॉल्ट वैल्यू मोड `:inspect` को अपडेट कर सकते हैं और `colors: [syntax: :dark]` जैसे options भी दे सकते हैं।

उदाहरण के लिए:

```elixir
num = 5
IO.inspect(num, label: "My number", pretty: true, limit: 2)
```

Output:

```
My number: 5
```

और यदि आप options का प्रयोग करना चाहते हैं:

```
My number: 5
-----pretty------
My number: 5
```

## गहराई में दूर्गाम

Debug output प्रिंट करना एक प्रोग्रामर के लिए अधिक से अधिक फायदेमंद हो सकता है। इससे आप अपने कोड में गहराई से समझ पाएंगे, त्रुटियों का पता लगाएंगे और मुश्किल स्थितियों को सुलझाएंगे। यह एक शक्तिशाली debugging टूल है जिससे आप अपने कोड को सुधार सकते हैं और प्रोग्रामिंग कौशल को सुधार सकते हैं।

## देखें भी

- [Elixir ऑफिशियल दस्तावेज़](https://hexdocs.pm/elixir/IO.html#inspect/2)
- [Debug टूलसेट का प्रयोग करना](https://medium.com/@johnnyrocket/debugging-in-elixir-with-inspect-56b4bd57991f)
- [Logging कैसे करें](https://vaidehijoshi.github.io/blog/2015/12/28/logging-in-elixir-applications/)