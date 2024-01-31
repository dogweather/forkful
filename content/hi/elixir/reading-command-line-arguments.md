---
title:                "कमांड लाइन आर्गुमेंट्स पढ़ना"
date:                  2024-01-20T17:56:06.239138-07:00
model:                 gpt-4-1106-preview
simple_title:         "कमांड लाइन आर्गुमेंट्स पढ़ना"

category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
Command line arguments वह जानकारी होती हैं जो हम एक प्रोग्राम को उसे चलाते समय देते हैं। ये इसलिए महत्वपूर्ण हैं क्योंकि इससे प्रोग्रामर्स को विभिन्न उपयोगकर्ता इनपुट्स पर आधारित परिणाम चलाने का मौका मिलता है।

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
