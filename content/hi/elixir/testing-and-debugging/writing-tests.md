---
title:                "टेस्ट लिखना"
aliases: - /hi/elixir/writing-tests.md
date:                  2024-02-03T19:31:54.228918-07:00
model:                 gpt-4-0125-preview
simple_title:         "टेस्ट लिखना"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
एलिक्सिर में परीक्षण लिखना आपके कोड के व्यवहार को मान्य करने के लिए स्वचालित स्क्रिप्ट्स बनाने की प्रक्रिया है। प्रोग्रामर गुणवत्ता की पुष्टि, रिग्रेसन्स को रोकने, और कोड रीफैक्टरिंग को सुविधाजनक बनाने के लिए ऐसा करते हैं, जिससे विकास प्रक्रिया अधिक विश्वसनीय और कार्यकुशल बनती है।

## कैसे करें:
एलिक्सिर अपने निर्मित परीक्षा ढांचे के रूप में ExUnit का उपयोग करता है, जो अत्यंत शक्तिशाली और उपयोग में आसान है। यहाँ एक बुनियादी उदाहरण है:

1. अपनी एलिक्सिर परियोजना की `test` निर्देशिका में एक नई परीक्षा फाइल बनाएँ। उदाहरण के लिए, यदि आप एक मॉड्यूल नाम `MathOperations` का परीक्षण कर रहे हैं, तो आपकी परीक्षा फाइल `test/math_operations_test.exs` हो सकती है।

```elixir
# test/math_operations_test.exs
defmodule MathOperationsTest do
  use ExUnit.Case

  # यह एक सरल परीक्षण मामला है जो जोड़ने की क्रिया की जांच करता है
  test "दो संख्याओं का जोड़" do
    assert MathOperations.add(1, 2) == 3
  end
end
```

अपने परीक्षण चलाने के लिए, अपने टर्मिनल में `mix test` कमांड का उपयोग करें। यदि `MathOperations.add/2` फ़ंक्शन सही ढंग से दो संख्याओं को जोड़ता है, तो आप इसी तरह का आउटपुट देखेंगे:

```
..

समाप्त हुआ 0.03 सेकंड में
1 परीक्षण, 0 विफलताएँ
```

बाहरी सेवाओं या API के साथ परीक्षणों के लिए, आप वास्तविक सेवाओं को हिट करने से बचने के लिए `mox` जैसे मॉक लाइब्रेरियों का उपयोग करना चाहेंगे:

1. `mix.exs` में अपने निर्भरताओं में `mox` जोड़ें:

```elixir
defp deps do
  [
    {:mox, "~> 1.0.0", only: :test},
    # अन्य deps...
  ]
end
```

2. अपने परीक्षा सहायक में एक मॉक मॉड्यूल परिभाषित करें (`test/test_helper.exs`):

```elixir
Mox.defmock(HTTPClientMock, for: HTTPClientBehaviour)
```

3. अपने परीक्षण मामले में मॉक का उपयोग करें:

```elixir
# test/some_api_client_test.exs
defmodule SomeAPIClientTest do
  use ExUnit.Case
  import Mox

  # यह Mox को यह बताता है कि यह मॉक अपेक्षित के अनुसार बुलाया गया था
  setup :verify_on_exit!

  test "API से डेटा प्राप्त करता है" do
    # मॉक प्रतिक्रिया सेटअप करें
    expect(HTTPClientMock, :get, fn _url -> {:ok, "Mocked response"} end)
    
    assert SomeAPIClient.get_data() == "Mocked response"
  end
end
```

`mix test` चलाते समय, यह सेटअप आपको वास्तविक बाहरी निर्भरताओं से अपने यूनिट परीक्षणों को अलग करने की अनुमति देता है, अपने स्वयं के कोड के व्यवहार पर ध्यान केंद्रित करता है। यह पैटर्न सुनिश्चित करता है कि आपके परीक्षण जल्दी और विश्वसनीय रूप से चलते हैं, बाहरी सेवा की स्थिति या इंटरनेट कनेक्टिविटी के बावजूद।
