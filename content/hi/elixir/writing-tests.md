---
title:                "परीक्षण लिखना"
date:                  2024-01-19
html_title:           "Arduino: परीक्षण लिखना"
simple_title:         "परीक्षण लिखना"

category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
टेस्ट लिखना कोड के छोटे टुकड़ों का परीक्षण करने की प्रक्रिया है। प्रोग्रामर्स इसे बग्स की पहचान करने, सॉफ्टवेयर की गुणवत्ता बढ़ाने और बाद में आसानी से मॉडिफिकेशन के लिए करते हैं।

## How to: (कैसे करें)
```elixir
defmodule MathTest do
  use ExUnit.Case
  
  # एक साधारण टेस्ट केस
  test "संख्याओं का योग" do
    assert 4 + 2 == 6
  end
end
```
जब ये टेस्ट रन होगा, इसका आउटपुट कुछ ऐसा होगा:
```
1 test, 0 failures
```

अगर हम गलती से बदलाव करें:
```elixir
test "संख्याओं का योग" do
  assert 4 + 2 == 7
end
```
तो आउटपुट ऐसा होगा:
```
1 test, 1 failure
```

## Deep Dive (गहराई में जानकारी)
एलिक्सिर में टेस्टिंग के लिए ExUnit फ्रेमवर्क का इस्तेमाल होता है, जो एक बिल्ट-इन फीचर है। हिस्टोरिकल संदर्भ में, टेस्ट ड्रिवन डेवलपमेंट (TDD) एक पॉपुलर अप्रोच है जिसे 2000 के दशक से ज्यादा अपनाया जा रहा है। एलिक्सिर में टेस्ट मैक्रो का इस्तेमाल करके टेस्ट केसेज को आसान बनाया जा सकता है, और एसिंक्रोनस टेस्टिंग के जरिए परीक्षण को तेज किया जाता है। विकल्पों में Property-based टेस्टिंग जैसे StreamData लाइब्रेरी शामिल हैं, जो रेंडमाइज्ड डेटा के साथ टेस्ट चलाती हैं।

## See Also (और देखें)
- [Elixir School Testing](https://elixirschool.com/en/lessons/basics/testing/)
- [ExUnit Documentation](https://hexdocs.pm/ex_unit/ExUnit.html)
- [Introduction to Mix](https://elixir-lang.org/getting-started/mix-otp/introduction-to-mix.html) 
- [Property Based Testing with StreamData in Elixir](https://elixirschool.com/en/lessons/libraries/stream-data/)
