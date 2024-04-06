---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:03.968213-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: Elixir \u092E\u0947\
  \u0902 \u0928\u093F\u0930\u094D\u092E\u093F\u0924 YAML \u0938\u092E\u0930\u094D\u0925\
  \u0928 \u0936\u093E\u092E\u093F\u0932 \u0928\u0939\u0940\u0902 \u0939\u0948\u0964\
  \ \u0939\u093E\u0932\u093E\u0902\u0915\u093F, \u0906\u092A \u0924\u0940\u0938\u0930\
  \u0947-\u092A\u0915\u094D\u0937 \u0915\u0940 \u0932\u093E\u0907\u092C\u094D\u0930\
  \u0947\u0930\u0940 \u091C\u0948\u0938\u0947 \u0915\u093F `yamerl` \u092F\u093E `yaml_elixir`\
  \ \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930 YAML \u0915\u0947 \u0938\
  \u093E\u0925 \u0915\u093E\u092E\u2026"
lastmod: '2024-04-05T21:53:53.779402-06:00'
model: gpt-4-0125-preview
summary: "Elixir \u092E\u0947\u0902 \u0928\u093F\u0930\u094D\u092E\u093F\u0924 YAML\
  \ \u0938\u092E\u0930\u094D\u0925\u0928 \u0936\u093E\u092E\u093F\u0932 \u0928\u0939\
  \u0940\u0902 \u0939\u0948\u0964 \u0939\u093E\u0932\u093E\u0902\u0915\u093F, \u0906\
  \u092A \u0924\u0940\u0938\u0930\u0947-\u092A\u0915\u094D\u0937 \u0915\u0940 \u0932\
  \u093E\u0907\u092C\u094D\u0930\u0947\u0930\u0940 \u091C\u0948\u0938\u0947 \u0915\
  \u093F `yamerl` \u092F\u093E `yaml_elixir` \u0915\u093E \u0909\u092A\u092F\u094B\
  \u0917 \u0915\u0930 YAML \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\
  \u0930 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902\u0964 \u092F\u0939\u093E\u0901\
  , \u0939\u092E \u0907\u0938\u0915\u0947 \u0909\u092A\u092F\u094B\u0917 \u092E\u0947\
  \u0902 \u0906\u0938\u093E\u0928\u0940 \u0914\u0930 \u0935\u094D\u092F\u093E\u092A\
  \u0915 \u0938\u0941\u0935\u093F\u0927\u093E\u0913\u0902 \u0915\u0947 \u0932\u093F\
  \u090F `yaml_elixir` \u092A\u0930 \u0927\u094D\u092F\u093E\u0928 \u0915\u0947\u0902\
  \u0926\u094D\u0930\u093F\u0924 \u0915\u0930\u0947\u0902\u0917\u0947\u0964 \u0938\
  \u092C\u0938\u0947 \u092A\u0939\u0932\u0947, \u0905\u092A\u0928\u0940 mix.exs \u0928\
  \u093F\u0930\u094D\u092D\u0930\u0924\u093E\u0913\u0902 \u092E\u0947\u0902 `yaml_elixir`\
  \ \u091C\u094B\u0921\u093C\u0947\u0902."
title: "YAML \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\
  \u093E"
weight: 41
---

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
