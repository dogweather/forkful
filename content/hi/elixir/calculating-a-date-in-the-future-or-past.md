---
title:                "Elixir: भविष्य या भूतकाल में दिनांक की गणना"
programming_language: "Elixir"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# क्यों 
एक देश के समय के साथ खेलने के लिए डेटा की गणना करना अपने प्रोग्रामिंग को बेहतर और सुव्यवस्थित बनाने के लिए बहुत उपयोगी हो सकता है।

# कैसे करें 
आप कैसे किसी दिन को आर्थिक वर्ष में, आने वाली तारीख को अनुमान लगाने के लिए कोडिंग उदाहरण और नमूना उत्पाद के साथ एक उद्धरण प्रस्तुत कर सकते हैं।

```elixir
defmodule DateCalculator do
  def calculate(date, years) do
    date
    |> Timex.add(years: years)
    |> Timex.format("{Year}-{Month}-{Day}")
  end
end

IO.inspect(DateCalculator.calculate({2020, 12, 25}, 1))
# Output: "2021-12-25"
```

# गहराई तक 
कोडिंग उदाहरण के साथ, और भी समझाने के लिए एक गहराई बात करते हुए, हम प्रोग्रामिंग में एक दिन को गणितीय रूप से कैसे उपयोग कर सकते हैं। आप अन्य प्रोग्राममंद फंक्शन में भी दिन को अनुमान लगाने के लिए उपयोग कर सकते हो।

# आगे देखें 
अधिक जानकारी के लिए, आप इन लिंक्स को देख सकते हैं: 
- [Elixir में दिनों को गणितीय रूप से कैसे उपयोग करें](https://elixirschool.com/blog/calendar-in-elixir/)
- [Timex लाइब्रेरी के बारे में और भी जानकारी](https://hexdocs.pm/timex/Timex.html)
- [Elixir में वैयक्तिक भाषाओं का उपयोग](https://hexdocs.pm/elixir/Kernel.SpecialForms.html)