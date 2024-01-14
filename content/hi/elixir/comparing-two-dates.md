---
title:                "Elixir: दो तारीखों का तुलना करना"
programming_language: "Elixir"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/comparing-two-dates.md"
---

{{< edit_this_page >}}

## क्यों

किसी तारीखों को तुलना करने में शामिल होने के लिए *क्यों* किसी को ज्यादातर इंसान द्वारा प्रयुक्त जॉडा काल्पनिक क्षेत्र के रूप में खेलने की क्षमता को बढ़ाने में मदद कर सकती है।

## कैसे करें

कूटमजगर और कोड प्रवर्तन जैसे विषयों में मकिंग के दौरान यह फायदेमंद हो सकता है, इस उदाहरण में, यह तारीखों को तुलना करने में कैसे मदद करता है:

```Elixir
defmodule DateComparison do
  def compare_dates(date1, date2) do
    if date1 > date2 do
      IO.puts "#{date1} is after #{date2}"
    else
      IO.puts "#{date2} is after #{date1}"
    end
  end
end

DateComparison.compare_dates(~D[2021-06-05], ~D[2021-06-01])
```

आउटपुट:
2021-06-05 is after 2021-06-01

## गहराई में जाएं

जैसा कि पहले भी उल्लेख किया गया है, तारीखों को तुलना करना बहुत महत्वपूर्ण हो सकता है जब हम बेहतर लोग बनने केंद्रित होते हैं। यह ट्यूटोरियल कुछ आभासी उदाहरण तारीख के मामले के लिए तारीख अपडेट करने और दो अलग प्रकार की तारीखों के बीच तुलना करने के लिए कुछ महत्वपूर्ण कूटस्थितियों को शामिल कर सकता है।

## इसके अलावा देखें

[अमीताभ सिंह द्वारा लिखित एक हार्डवेयर समन्वय समाधान के बारे में जानकारी](https://elixir-lang.org/getting-started/introduction.html)

[0xAX की ब्लॉग पोस्ट जो Elixir के ग्राहक को होस्ट करते हैं](https://elixir-lang.org/getting-started/introduction.html)

[व्यावसायिक Elixir दस्तावेज़ की जांच जानकारी](https://hexdocs.pm/elixir/getting-started.html)