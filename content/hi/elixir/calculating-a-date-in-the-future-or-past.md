---
title:                "भविष्य या भूतकाल में एक तारीख की गणना"
html_title:           "Elixir: भविष्य या भूतकाल में एक तारीख की गणना"
simple_title:         "भविष्य या भूतकाल में एक तारीख की गणना"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## इलिक्सियर (मौजूदा संस्करण) प्रोग्रामिंग कालम हिन्दी पाठकों के लिए

अभी तक कई भाषाओं में विकसित की गई, इलिक्सियर एक फंक्शनल प्रोग्रामिंग भाषा है जो अधक शक्तिशाली और प्रभावी है। यह दोनों concurrent तथा parallel programming को समर्थन करता है जो सामान्य प्रोग्रामिंग भाषाओं के मुकाबले ज्यादा सुविधाओं और क्षमताओं का लाभ उठाती है।

## क्यों

कोई भी व्यक्ति तारीखों की गणना करने का संभवतः कारण एक दूसरे से पीछे या आगे चला जाएगा। यह भविष्य की योजनाओं को सुचारू बनाने, समय का अनुमान लगाने, और कार्यक्रमों के साथ सार्वजनिक अनुसूचियों को करने का एक अच्छा तरीका हो सकता है।

## कैसे करें

```elixir
# An example of calculating the date 7 days after a given date
  def calculate_future(date) do
    date |> Timex.add(7, "days") |> Timex.parse!("{YYYY}-{0M}-{0D}")
  end

# Output: "2021-09-08"
```

```elixir
# An example of calculating the date 1 month before a given date
  def calculate_past(date) do
    date |> Timex.sub(1, "months") |> Timex.parse!("{YYYY}-{0M}-{0D}")
  end

# Output: "2021-07-02"
```

## गहराई में जाएं

किसी भी तारीख की संख्या निर्धारित करने के लिए इलिक्सियर मेंइस्तेमाल किया जाता है। यह तारीखों को बस 24 घंटे और 360 दिन की तरह नहीं बल्कि अन्य प्राथमिक यूनिट जैसे महीने, वर्ष और सप्ताह में भी रूपांतरित किया जा सकता है। इसके साथ साथ, Timex लाइब्रेरी  अत्यधिकन सरल और सुविधाजनक बनाती है, जबकि अन्य इलिक्सियर फ्रेमवर्क समय कॉन्फ