---
title:                "TOML के साथ काम करना"
aliases:
- /hi/elixir/working-with-toml/
date:                  2024-01-26T04:21:40.210811-07:00
model:                 gpt-4-0125-preview
simple_title:         "TOML के साथ काम करना"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/working-with-toml.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
TOML के साथ काम करना का अर्थ है Elixir का उपयोग करके TOML (टॉम की स्पष्ट, न्यूनतम भाषा) डाटा का पार्सिंग और उत्पन्न करना। प्रोग्रामर्स इसे कॉन्फ़िगरेशन फाइलों को संभालने के लिए उपयोग करते हैं क्योंकि TOML पढ़ने में आसान, पार्स करने में सरल होता है और यह एक हैश डाटा स्ट्रक्चर के साथ अच्छी तरह मैप करता है।

## कैसे करें:
सबसे पहले, अपनी मिक्स निर्भरताओं में एक TOML पार्सर जोड़ें। यह उदाहरण `toml-elixir` का उपयोग करता है:

```elixir
def deps do
  [
    {:toml_elixir, "~> 2.0"}
  ]
end
```

एक TOML फाइल पढ़ें:

```elixir
{:ok, toml_data} = File.read("config.toml")
{:ok, parsed_data} = TomlElixir.parse(toml_data)
```

Elixir डाटा को TOML में परिवर्तित करने के लिए:

```elixir
data = %{title: "TOML उदाहरण", owner: %{name: "Tom Preston-Werner"}}
toml_string = TomlElixir.encode(data)
```

नमूना आउटपुट:

```elixir
"title = \"TOML उदाहरण\"\n\n[owner]\nname = \"Tom Preston-Werner\"\n"
```

## गहराई में
TOML का निर्माण GitHub के सह-संस्थापक टॉम प्रेस्टन-वेर्नर द्वारा कॉन्फ़िगरेशन फाइलों के उपयोग के लिए किया गया था। यह XML की तुलना में अधिक सरल और YAML की तुलना में अधिक संक्षिप्त होने के लिए डिज़ाइन किया गया है, साथ ही स्थिरता बनाए रखते हुए।

विकल्पों में JSON, YAML, और INI फाइलें शामिल हैं, प्रत्येक में मानवीय पठनीयता और डाटा स्ट्रक्चर संगतता में उनके समझौते होते हैं। TOML सारणीबद्ध डाटा और डाटा के नेस्टेड ग्रुपिंग को स्पष्ट रूप से प्रस्तुत करने में उत्कृष्ट है।

Elixir में, TOML हैंडलिंग डिकोडिंग और एनकोडिंग लाइब्रेरीज पर निर्भर करती है, जो TOML स्ट्रिंग्स को Elixir मैप्स में और इसके विपरीत में परिवर्तित करती हैं। पार्सिंग TOML के सिंटैक्स नियमों को मैच करके और उन्हें Elixir के डाटा प्रकारों में परिवर्तित करके काम करती है। एनकोडिंग इसके विपरीत करती है, इससे Elixir के डाटा प्रकारों को वैध TOML सिंटैक्स में मैप किया जाता है।

## यह भी देखें
- TOML भाषा: https://toml.io/en/
- `toml-elixir` GitHub रिपॉजिटरी: https://github.com/bitwalker/toml-elixir
- `toml-elixir` के लिए Hex पैकेज विवरण: https://hex.pm/packages/toml_elixir
