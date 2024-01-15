---
title:                "प्रोग्रामिंग में टेस्ट लिखना"
html_title:           "Elixir: प्रोग्रामिंग में टेस्ट लिखना"
simple_title:         "प्रोग्रामिंग में टेस्ट लिखना"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/writing-tests.md"
---

{{< edit_this_page >}}

## क्यों

टेस्ट लिखने में लोगों के लिए जो भी हो, आमतौर पर कोड को चेक करने के लिए अधिक सुरक्षित बनाने का सुनहरा तरीका है। यह आपके कोड में त्रुटियों को पहचानने और सुधार करने में मदद कर सकता है।

## कैसे करें

```Elixir
test "गणना के रूप में अंकों को जोड़ना" do
  assert 2 + 2 == 4
end
```

ऊपर दिए गए उदाहरण में, हमने दो अंकों को जोड़कर सत्यापित किया है कि यह 4 है। टेस्ट कोड ब्लॉक के भीतर `test` फंक्शन का उपयोग करके हम अपने कोड को टेस्ट करने के लिए सिस्टम को बता सकते हैं। तब हम `assert` विधि का उपयोग करके अपने अपेक्षित परिणाम को सत्यापित करते हैं।

## गहराई में जाओ

टेस्ट लिखने के बारे में अधिक जानने के लिए, हम अपने कोड को साफ और सुधारने के लिए विभिन्न टेस्ट प्रकारों को समझने की आवश्यकता होती है। यह आपको अपने कोड को समझने में और अधिक मजबूत बनाने में मदद कर सकता है। आप इस लेख को पढ़कर `ExUnit` के बारे में और अधिक जानकारी पा सकते हैं।

## इस देखें

[ExUnit दस्तावेज़ीकरण](https://hexdocs.pm/ex_unit/ExUnit.html)

[Test-Driven Development के फायदे](https://medium.freecodecamp.org/what-is-test-driven-development-in-elixir-and-why-should-you-use-it-fab23c5523c7)

[उदाहरण आधारित विकास का मूल तत्व](https://medium.com/@noelrap/example-driven-development-the-essential-formula-for-success-in-software-8d1f8a2f9eae)