---
title:                "यामल के साथ काम करना"
html_title:           "Elixir: यामल के साथ काम करना"
simple_title:         "यामल के साथ काम करना"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/working-with-yaml.md"
---

{{< edit_this_page >}}

"## क्या और क्यों?"
लेखकों को अक्सर YAML लेखन और पार्स करने के लिए इस्तेमाल किया जाता है। YAML (YAML Ain't a Markup Language) टेक्स्ट डेटा से संबंधित स्टैंडर्ड फॉर्मैट है जो आसानी से इंन्टरप्रिट होता है और जैसे पठनीय होता है।

"## कैसे करें?"
```Elixir
# YAML को अनामित मान की मदद से रीड करें
YAML.load("नाम: जॉन, उम्र: 30")
# %{नाम: "जॉन", उम्र: 30}

# विशेषताओं के साथ YAML फाइल लिखें
YAML.dump(%{नाम: "जॉन", उम्र: 30})
# "---\nनाम: जॉन\nउम्र: 30\n..."

# कस्टम विशेषताओं के साथ पार्स करें
YAML.load(<<"db:\n  username: 'जॉन'\n  password: 'पासवर्ड'">>)
# %{db: %{username: "जॉन", password: "पासवर्ड"}}
```

"## गहराई में जाओ"
YAML एक आसान और पठनीय फॉर्मैट है जो डेटा को संरचित करने के लिए बहुत सारे विशेषताओं की अनुमति देता है। यह फॉर्मैट फ़ाइलों में इनसेपरेबल लग सकता है, जो कोड को पढ़ने और लिखने को आसान बनाता है। इसके अलावा, यह अन्य फॉर्मेट्स (XML, JSON) से भी सुपीडी है।

"## इससे संबंधित जानकारी"
- [YAML की विस्तृत जानकारी](https://yaml.org/)
- [यूट्यूब पर YAML के बारे में तुतोरियल](https://www.youtube.com/results?search_query=yaml+tutorial)
- [Elixir के बारे में अधिक जानकारी के लिए अधिकांश](https://elixir-lang.org/getting-started/introduction.html)