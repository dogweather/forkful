---
title:                "YAML के साथ काम करना"
aliases:
- hi/lua/working-with-yaml.md
date:                  2024-02-03T19:26:46.768397-07:00
model:                 gpt-4-0125-preview
simple_title:         "YAML के साथ काम करना"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/lua/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

YAML, जिसका पूरा नाम "YAML Ain't Markup Language" है, एक मानव-पठनीय डेटा सीरियलाइजेशन मानक है जो अक्सर कॉन्फिगरेशन फाइलों और भाषाओं के बीच डेटा विनिमय के लिए प्रयोग किया जाता है। प्रोग्रामर YAML का उपयोग इसकी सादगी और पठनीयता के कारण करते हैं, जिससे यह सेटिंग्स, विविध एप्लिकेशन कॉन्फिगरेशंस, या ऐसी सामग्री के लिए पसंदीदा विकल्प बन जाता है जिसे गैर-प्रोग्रामरों द्वारा संपादित किया जाना चाहिए।

## कैसे करें:

Lua में YAML के लिए निर्मित समर्थन नहीं है, लेकिन आप `lyaml` जैसी तृतीय-पक्ष पुस्तकालयों का उपयोग करके YAML फाइलों के साथ काम कर सकते हैं। यह पुस्तकालय Lua के साथ YAML डेटा को एन्कोडिंग और डीकोडिंग करने की सुविधा देता है। पहले, आपको LuaRocks के माध्यम से `lyaml` स्थापित करने की आवश्यकता होगी, Lua का पैकेज मैनेजर:

```bash
luarocks install lyaml
```

### YAML को डीकोड करना:

मान लीजिए आपके पास `config.yaml` नामक फाइल में निम्नलिखित YAML सामग्री है:

```yaml
database:
  host: localhost
  port: 3306
  username: user
  password: pass
```

आप इस YAML फाइल को निम्न कोड के साथ एक Lua तालिका में डीकोड कर सकते हैं:

```lua
local yaml = require('lyaml')
local file = io.open("config.yaml", "r")
local content = file:read("*all")
file:close()

local data = yaml.load(content)
for k,v in pairs(data.database) do
  print(k .. ": " .. v)
end
```

जब आप यह स्क्रिप्ट चलाएंगे, तो यह आउटपुट देगा:

```output
host: localhost
port: 3306
username: user
password: pass
```

### YAML को एन्कोड करना:

Lua तालिकाओं को YAML प्रारूप में एन्कोड करने के लिए, आप `lyaml` द्वारा प्रदान किए गए `dump` फ़ंक्शन का उपयोग करते हैं। मान लें आप निम्नलिखित Lua तालिका का YAML प्रतिनिधित्व बनाना चाहते हैं:

```lua
local data = {
  website = {
    name = "Example",
    owner = "Jane Doe",
    metadata = {
      creation_date = "2023-01-01",
      tags = {"blog", "personal", "lua"}
    }
  }
}

local yaml = require('lyaml')
local yaml_data = yaml.dump({data})
print(yaml_data)
```

आउटपुट YAML होगा:

```yaml
- website:
    metadata:
      creation_date: '2023-01-01'
      tags: [blog, personal, lua]
    name: Example
    owner: Jane Doe
```

इन पैटर्नों का पालन करते हुए, Lua प्रोग्रामर विभिन्न एप्लिकेशनों के लिए YAML डेटा का प्रभावी ढंग से प्रबंधन कर सकते हैं। YAML के साथ ये क्रियाएँ अन्य प्रणालियों के साथ या सीधे अन्य प्रणालियों के साथ सुचारू रूप से बातचीत करने वाले बहुमुखी Lua एप्लिकेशनों के विकास के लिए महत्वपूर्ण हैं।
