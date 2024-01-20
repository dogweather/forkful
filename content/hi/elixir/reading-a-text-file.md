---
title:                "एक पाठ फ़ाइल पढ़ना"
html_title:           "Bash: एक पाठ फ़ाइल पढ़ना"
simple_title:         "एक पाठ फ़ाइल पढ़ना"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/reading-a-text-file.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

एक पाठ फ़ाइल को पढ़ना मतलब होता है कि हम किसी फ़ाइल की कंटेंट्स को लोड करके उसे किसी प्रोग्राम में इस्तेमाल करते हैं। प्रोग्रामर्स इसे करते हैं ताकि वे डेटा को दोबारा इस्तेमाल कर सकें और विशेष तरीके से प्रसंस्करण कर सकें।

## कैसे:

नीचे दी गई कोडिंग उदाहरण से आप सीख सकते हैं कि कैसे Elixir में टेक्स्ट फ़ाइल को पढ़ा जा सकता है।

```Elixir
defmodule ReadFile do
  def read_text_file(file_path) do
    {:ok, content} = File.read(file_path)
    String.split(content, "\n")
  end
end
```

इसे इस्तेमाल करने पर, यदि आपकी फ़ाइल का नाम "sample.txt" है, तो आप निम्नलिखित तरीके से उपयोग कर सकते हैं:

```Elixir
IO.inspect(ReadFile.read_text_file("sample.txt"))
```

और यह आपको एक लिस्ट देगा जिसमें वाक्यांशों की एक सूची होगी। 

## गहराई विवेचन:

Elixir में फ़ाइल को पढ़ने के जीवनायास तरीके का उपयोग करना बहुत ही सरल है, जो कि Erlang VM पर निर्माणित हुआ है। इसके विकल्पों में इन-मेमोरी और बाहरी भंडारण को शामिल किया गया है। जबकि इसके प्रमुख विवरण में विभाजन, एन्कोडिंग, और इंडेक्सिंग समेत होते हैं।

## यह भी देखें:

- [Elixir Getting Started Guide](https://elixir-lang.org/getting-started/introduction.html): इसमें आपको Elixir के बारे में बुनियादी जानकारी मिलेगी।
- [Elixir File Module Documentation](https://hexdocs.pm/elixir/File.html): यह Elixir के फ़ाइल मॉड्यूल का विस्तृत प्रलेखन है।
- [Reading Files in Elixir](https://medium.com/@andreichernykh/elixir-read-file-b2b3e4c2cf9e): इस ब्लॉग पोस्ट में एलिक्सिर में फ़ाइल पढ़ने के बारे में विस्तृत जानकारी दी गई है।