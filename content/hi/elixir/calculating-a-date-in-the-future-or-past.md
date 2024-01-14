---
title:    "Elixir: भविष्य या भूतकाल में दिनांक की गणना"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# क्यों

अभी कल्कुलेशन को क्यों संबोधित करें।

एक्सिर्ली के साथ आप प्रोग्रामिंग का आनंद ले सकते हैं।

# जैसे करें

```
Elixir Code Example
defmodule DateCalculator do
  # Calculate future date
  def calculate_future_date(current_date, num_days) do
    Calendar.ISO.date_from_erl_tuple Calendar.ISO.to_erl(current_date) |> elem(1) +
      num_days
  end
end

IO.inspect DateCalculator.calculate_future_date({2019, 10, 20}, 5)
# Output: {2019, 10, 25}
```

```
Elixir Code Example
defmodule DateCalculator do
  # Calculate past date
  def calculate_past_date(current_date, num_days) do
    Calendar.ISO.date_from_erl_tuple Calendar.ISO.to_erl(current_date) |> elem(1)
      - num_days
  end
end

IO.inspect DateCalculator.calculate_past_date({2019, 10, 20}, 5)
# Output: {2019, 10, 15}
```

# गहराई से जानना

कल्कुलेशन को भविष्य या भूतकाल की तारीख को प्राप्त करने के लिए किसी भी आगामी समय में काम आ सकता है। आप वर्तमान दिनांक में एक अंक और एलिक्सिर कोड के साथ नंबर दिया जा सकता है और प्राप्त मंडलीस्थ तारीख को प्राप्त कर सकता है। इसके अलावा, आप आगामी समय में सटीक दिनांक को प्राप्त करने के लिए वीसी आज का कोड भी शामिल कर सकते हैं।

यह प्रक्रिया आपको प्रासंगिक और संबंधित दिन तारीख को जानने का एक अच्छा तरीका दे सकता है। इसके अलावा, आप इस कोड को अपने परियोजनाओं में भी शामिल कर सकते हैं।

# देखें भी

- [Elixir कोडिंग शैली गाइड](https://hexdocs.pm/elixir/master/readme.html)
- [Elixir मानक सार्वजनिकीकरण सहायता](https://hexdocs.pm/elixir/master/public-function-args.html)