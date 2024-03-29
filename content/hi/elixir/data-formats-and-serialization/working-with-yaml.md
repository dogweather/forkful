---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:03.968213-07:00
description: "YAML, \u091C\u093F\u0938\u0947 YAML Ain't Markup Language \u0915\u0947\
  \ \u0932\u093F\u090F \u091B\u094B\u091F\u093E \u0915\u093F\u092F\u093E \u0917\u092F\
  \u093E \u0939\u0948, \u090F\u0915 \u092E\u093E\u0928\u0935-\u092A\u0920\u0928\u0940\
  \u092F \u0921\u0947\u091F\u093E \u0938\u0940\u0930\u093F\u092F\u0932\u093E\u0907\
  \u091C\u0947\u0936\u0928 \u092E\u093E\u0928\u0915 \u0939\u0948 \u091C\u094B \u0906\
  \u092E\u0924\u094C\u0930 \u092A\u0930 \u0915\u0949\u0928\u094D\u092B\u093C\u093F\
  \u0917\u0930\u0947\u0936\u0928 \u092B\u093E\u0907\u0932\u094B\u0902 \u0914\u0930\
  \ \u0935\u093F\u092D\u093F\u0928\u094D\u0928 \u0921\u0947\u091F\u093E\u2026"
lastmod: '2024-03-13T22:44:51.772419-06:00'
model: gpt-4-0125-preview
summary: "YAML, \u091C\u093F\u0938\u0947 YAML Ain't Markup Language \u0915\u0947 \u0932\
  \u093F\u090F \u091B\u094B\u091F\u093E \u0915\u093F\u092F\u093E \u0917\u092F\u093E\
  \ \u0939\u0948, \u090F\u0915 \u092E\u093E\u0928\u0935-\u092A\u0920\u0928\u0940\u092F\
  \ \u0921\u0947\u091F\u093E \u0938\u0940\u0930\u093F\u092F\u0932\u093E\u0907\u091C\
  \u0947\u0936\u0928 \u092E\u093E\u0928\u0915 \u0939\u0948 \u091C\u094B \u0906\u092E\
  \u0924\u094C\u0930 \u092A\u0930 \u0915\u0949\u0928\u094D\u092B\u093C\u093F\u0917\
  \u0930\u0947\u0936\u0928 \u092B\u093E\u0907\u0932\u094B\u0902 \u0914\u0930 \u0935\
  \u093F\u092D\u093F\u0928\u094D\u0928 \u0921\u0947\u091F\u093E\u2026"
title: "YAML \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\
  \u093E"
---

{{< edit_this_page >}}

## क्या और क्यों?

YAML, जिसे YAML Ain't Markup Language के लिए छोटा किया गया है, एक मानव-पठनीय डेटा सीरियलाइजेशन मानक है जो आमतौर पर कॉन्फ़िगरेशन फाइलों और विभिन्न डेटा संरचनाओं के साथ भाषाओं के बीच डेटा आदान-प्रदान के लिए उपयोग किया जाता है। प्रोग्रामर इसकी सादगी और जटिल पदानुक्रमित डेटा को आसानी से प्रस्तुत करने की क्षमता के कारण इसका इस्तेमाल करते हैं।

## कैसे करें:

Elixir में निर्मित YAML समर्थन शामिल नहीं है। हालांकि, आप तीसरे-पक्ष की लाइब्रेरी जैसे कि `yamerl` या `yaml_elixir` का उपयोग कर YAML के साथ काम कर सकते हैं। यहाँ, हम इसके उपयोग में आसानी और व्यापक सुविधाओं के लिए `yaml_elixir` पर ध्यान केंद्रित करेंगे।

सबसे पहले, अपनी mix.exs निर्भरताओं में `yaml_elixir` जोड़ें:

```elixir
defp deps do
  [
    {:yaml_elixir, "~> 2.9"}
  ]
end
```

फिर, नई निर्भरता को प्राप्त करने के लिए `mix deps.get` चलाएँ।

### YAML पढ़ना

एक साधारण YAML फाइल, `config.yaml`, जो इस प्रकार दिखती है:

```yaml
database:
  adapter: postgres
  username: user
  password: pass
```

आप इस YAML फाइल को पढ़ सकते हैं और इसे एक Elixir मानचित्र में परिवर्तित कर सकते हैं इस प्रकार:

```elixir
defmodule Config do
  def read do
    {:ok, content} = YamlElixir.read_from_file("config.yaml")
    content
  end
end

# नमूना उपयोग
Config.read()
# आउटपुट: 
# %{
#   "database" => %{
#     "adapter" => "postgres",
#     "username" => "user",
#     "password" => "pass"
#   }
# }
```

### YAML लिखना

एक मानचित्र को वापस YAML फाइल में लिखने के लिए:

```elixir
defmodule ConfigWriter do
  def write do
    content = %{
      database: %{
        adapter: "mysql",
        username: "root",
        password: "s3cret"
      }
    }
    
    YamlElixir.write_to_file("new_config.yaml", content)
  end
end

# नमूना उपयोग
ConfigWriter.write()
# यह `new_config.yaml` को निर्दिष्ट सामग्री के साथ बनाएगा या उसे ओवरराइट करेगा
```

ध्यान दें कैसे `yaml_elixir` YAML फाइलों और Elixir डेटा संरचनाओं के बीच एक सरलानुवाद की अनुमति देता है, जिससे यह Elixir प्रोग्रामरों के लिए एक उत्कृष्ट विकल्प बनता है जिन्हें YAML डेटा के साथ काम करने की आवश्यकता होती है।
