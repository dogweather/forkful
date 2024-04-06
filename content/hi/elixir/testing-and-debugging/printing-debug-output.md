---
date: 2024-01-20 17:52:56.983129-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) Elixir\
  \ \u092E\u0947\u0902 \u0921\u0940\u092C\u0917 \u0906\u0909\u091F\u092A\u0941\u091F\
  \ \u092A\u094D\u0930\u093F\u0902\u091F \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\
  \u093F\u090F `IO.inspect/2` \u092F\u093E `IO.puts/1` \u0915\u093E \u0907\u0938\u094D\
  \u0924\u0947\u092E\u093E\u0932 \u0915\u093F\u092F\u093E \u091C\u093E\u0924\u093E\
  \ \u0939\u0948\u0964."
lastmod: '2024-04-05T21:53:53.751707-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) Elixir \u092E\u0947\
  \u0902 \u0921\u0940\u092C\u0917 \u0906\u0909\u091F\u092A\u0941\u091F \u092A\u094D\
  \u0930\u093F\u0902\u091F \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F\
  \ `IO.inspect/2` \u092F\u093E `IO.puts/1` \u0915\u093E \u0907\u0938\u094D\u0924\u0947\
  \u092E\u093E\u0932 \u0915\u093F\u092F\u093E \u091C\u093E\u0924\u093E \u0939\u0948\
  \u0964."
title: "\u0921\u0940\u092C\u0917 \u0906\u0909\u091F\u092A\u0941\u091F \u092A\u094D\
  \u0930\u093F\u0902\u091F \u0915\u0930\u0928\u093E"
weight: 33
---

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
