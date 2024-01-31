---
title:                "डीबग आउटपुट प्रिंट करना"
date:                  2024-01-20T17:52:56.983129-07:00
model:                 gpt-4-1106-preview
simple_title:         "डीबग आउटपुट प्रिंट करना"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
प्रिंटिंग डीबग आउटपुट का मतलब है कोड में चल रही गतिविधियों को कंसोल में दिखाना। प्रोग्रामर्स यह इसलिए करते हैं क्योंकि इससे एप्लिकेशन में समस्याओं का पता चलता है और उन्हें हल करने में आसानी होती है।

## How to: (कैसे करें:)
Elixir में डीबग आउटपुट प्रिंट करने के लिए `IO.inspect/2` या `IO.puts/1` का इस्तेमाल किया जाता है।

```elixir
# ऑब्जेक्ट डीबग करने के लिए
value = %{name: "Elixir", awesome: true}
IO.inspect(value)

# सिंपल मेसेज प्रिंट करने के लिए
IO.puts("Hello, Elixir!")
```

उपरोक्त कोड से मिलने वाला आउटपुट:
```
%{awesome: true, name: "Elixir"}
Hello, Elixir!
```

## Deep Dive (गहराई में जानकारी:)
`IO.inspect/2` फंक्शन को Elixir ने डीबगिंग के लिए डिज़ाइन किया है। इसे डाटा के स्ट्रक्चर को समझने में मदद मिलती है। Elixir में रनटाइम पर डिबग इनफार्मेशन जोड़ने के लिएभी अन्य टूल्स हैं, जैसे कि `:debugger` और `:observer` जो एर्लांग से आते हैं। `IO.puts/1` को मुख्यतः सीधे सीधे स्ट्रिंग मेसेजेस प्रिंट करने में इस्तेमाल किया जाता है।

## See Also (और जानें:)
- Elixir के आधिकारिक दस्तावेज़ [Elixir Documentation on IO](https://hexdocs.pm/elixir/IO.html)
- Erlang के डीबगर के बारे में [Erlang — Debugger](http://erlang.org/doc/apps/debugger/debugger_chapter.html)
