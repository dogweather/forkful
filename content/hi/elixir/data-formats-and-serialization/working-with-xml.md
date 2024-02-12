---
title:                "XML के साथ काम करना"
aliases:
- /hi/elixir/working-with-xml.md
date:                  2024-01-26T04:30:21.904409-07:00
model:                 gpt-4-0125-preview
simple_title:         "XML के साथ काम करना"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/working-with-xml.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
एलिक्सिर में XML के साथ काम करना का मतलब है XML डेटा को पार्स करना, बनाना, और संशोधित करना। प्रोग्रामर इसका सामना करते हैं क्योंकि यह वेब सेवाओं, कॉन्फिग फ़ाइलों, और पुरानी प्रणालियों में व्यापक है।

## कैसे करें:
एलिक्सिर अपनी स्टैंडर्ड लाइब्रेरी में XML पार्सिंग को शामिल नहीं करता है। SweetXML एक लोकप्रिय विकल्प है। यहां बताया गया है कि इसका उपयोग कैसे करें:

```elixir
# mix.exs में अपने निर्भरताओं में SweetXML जोड़ें
{:sweet_xml, "~> 0.6"}

# अपने कोड में
import SweetXml

xml = """
<note>
  <to>Tove</to>
  <from>Jani</from>
  <heading>Reminder</heading>
  <body>Don't forget me this weekend!</body>
</note>
"""

# XML पार्स करें
note = xml |> xpath(~x"//note")
to = xml |> xpath(~x"//note/to" |> inner_text())
IO.puts to # आउटपुट: Tove
```

## गहराई में जाने के लिए
XML, या Extensible Markup Language, 90 के दशक के अंत से रहा है। यह बड़बोला है लेकिन संरचित है—जटिल डेटा आदान-प्रदान के लिए आदर्श है। हालांकि JSON की लोकप्रियता इसकी सादगी के लिए बढ़ी, XML कई एंटरप्राइज़ और वित्तीय प्रणालियों में इसके अभिव्यक्तिक और मानकीकृत स्कीमाओं के लिए गहरा जमा हुआ है।

विकल्पों में शामिल हैं:
- ज्यादा हल्का, कम बड़बोला डेटा आदान-प्रदान के लिए JSON।
- विशेष रूप से आंतरिक प्रणालियों के लिए, बाइनरी सीरियलाइज्ड डेटा संचार के लिए Protobuf या Thrift।

मुख्य रूप से, एलिक्सिर के लिए XML लाइब्रेरीज पार्सिंग के लिए एर्लांग के :xmerl लाइब्रेरी का लाभ उठाती हैं, जो मजबूत समर्थन प्रदान करती है लेकिन अधिक आधुनिक दृष्टिकोणों की तुलना में कम सहज हो सकती है। जैसे-जैसे एलिक्सिर विकसित होता है, समुदाय संचालित लाइब्रेरीज जैसे कि SweetXML इन्हें एक और एलिक्सिर-एश सिंटैक्स के साथ लपेटते हैं, XML मैनिपुलेशन को अधिक सुलभ बनाते हैं।

## और भी देखें:
- Hex पर SweetXML: https://hex.pm/packages/sweet_xml
- XML पार्सिंग पर एलिक्सिर का दृष्टिकोण: https://elixir-lang.org/getting-started/mix-otp/dependencies-and-umbrella-projects.html
- अंतर्निहित XML हैंडलिंग के लिए xmerl दस्तावेज़ीकरण: http://erlang.org/doc/apps/xmerl/index.html
