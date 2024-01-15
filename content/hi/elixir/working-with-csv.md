---
title:                "कंप्यूटर प्रोग्रामिंग में सीएसवी से काम करना"
html_title:           "Elixir: कंप्यूटर प्रोग्रामिंग में सीएसवी से काम करना"
simple_title:         "कंप्यूटर प्रोग्रामिंग में सीएसवी से काम करना"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/working-with-csv.md"
---

{{< edit_this_page >}}

Hindi:

## क्यों

CSV के साथ काम करने में आपका समय बचाने और डेटा को आसानी से व्यवस्थित करने के लिए आपको तैयार होना चाहिए। Elixir में काम करने के लिए CSV।

## कैसे

`` `Elixir
CSV.parse ("फ़ाइल. csv") |> Enum. take (10) |> Enum.each (fn [row] -> IO.inspect (row) "
`` `

आपको पहले से ही टेक्स्ट फ़ाइल में ऑत्पुट दिखाने के लिए कोड फ़ाइल `। csv` को पारस्क कर सकते हैं। यदि आप `।csv` फ़ाइल से अर्जुना सूची अपने कम्प्यूटर पर ले रहे हैं-रख सकते हैं तो इस डेटा को देखने की कोशिश करें।

## गहराई समाधान

CSV काम करते समय, आप ह्रदयत उत्पादनित स्थाति, संज्ञान उत्पादनित गणना कर सकते हैं। पूर्व मान आप्तोंकनत्या चिह्नित फ़ाइल ऑल। कर सकते हैं।। जवाब काम करता है।

## देखें भी

- [Elixir मैनुअल](https://www.elixir-lang.org/getting-started/introduction.html)
- [CSV पैर्सर प्रकटीकरण्डम](https://github.com/beatrichartz/csv)
- [CSV ट्यूटोरियल](https://www.codecademy.com/learn/learn-csv)