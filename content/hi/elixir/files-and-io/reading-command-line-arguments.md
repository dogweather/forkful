---
date: 2024-01-20 17:56:06.239138-07:00
description: "How to: (\u0915\u0948\u0938\u0947:) Elixir \u092E\u0947\u0902 command\
  \ line arguments \u092A\u0922\u093C\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F\
  \ `System.argv/0` \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u093F\u092F\
  \u093E \u091C\u093E\u0924\u093E \u0939\u0948\u0964 \u0906\u0907\u090F \u090F\u0915\
  \ \u0938\u093E\u0927\u093E\u0930\u0923 \u0909\u0926\u093E\u0939\u0930\u0923 \u0915\
  \u0947 \u0938\u093E\u0925 \u0926\u0947\u0916\u0924\u0947 \u0939\u0948\u0902\u0964\
  ."
lastmod: '2024-03-13T22:44:51.764089-06:00'
model: gpt-4-1106-preview
summary: "Elixir \u092E\u0947\u0902 command line arguments \u092A\u0922\u093C\u0928\
  \u0947 \u0915\u0947 \u0932\u093F\u090F `System.argv/0` \u0915\u093E \u0909\u092A\
  \u092F\u094B\u0917 \u0915\u093F\u092F\u093E \u091C\u093E\u0924\u093E \u0939\u0948\
  \u0964 \u0906\u0907\u090F \u090F\u0915 \u0938\u093E\u0927\u093E\u0930\u0923 \u0909\
  \u0926\u093E\u0939\u0930\u0923 \u0915\u0947 \u0938\u093E\u0925 \u0926\u0947\u0916\
  \u0924\u0947 \u0939\u0948\u0902\u0964."
title: "\u0915\u092E\u093E\u0902\u0921 \u0932\u093E\u0907\u0928 \u0906\u0930\u094D\
  \u0917\u0941\u092E\u0947\u0902\u091F\u094D\u0938 \u092A\u0922\u093C\u0928\u093E"
weight: 23
---

## How to: (कैसे:)
Elixir में command line arguments पढ़ने के लिए `System.argv/0` का उपयोग किया जाता है। आइए एक साधारण उदाहरण के साथ देखते हैं।

```elixir
# greet.exs
defmodule Greeter do
  def main(args) do
    IO.puts "नमस्ते, #{Enum.join(args, " ")}!"
  end
end

Greeter.main(System.argv())
```

उपयोग करते हुए:

```
$ elixir greet.exs John Doe
नमस्ते, John Doe!
```

## Deep Dive (गहराई में जानकारी)
Command line arguments की सुविधा प्रोग्रामिंग के शुरुआती दिनों से है। Elixir में `System.argv/0` के अलावा `OptionParser` मॉड्यूल है जो अधिक जटिल ऑप्शन्स और flags को पार्स करने के काम आता है। Implementation के दौरान, यह ध्यान रखें कि command line arguments हमेशा string के रूप में पास होती हैं, इसलिए कभी-कभी आपको उन्हें उपयुक्त डेटा टाइप में बदलना पड़ सकता है।

## See Also (देखने योग्य स्रोत)
- [Elixir's Official Documentation for System.argv/0](https://hexdocs.pm/elixir/System.html#argv/0)
- [OptionParser Module Documentation](https://hexdocs.pm/elixir/OptionParser.html)
