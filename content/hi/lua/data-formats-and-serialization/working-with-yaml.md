---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:46.768397-07:00
description: "YAML, \u091C\u093F\u0938\u0915\u093E \u092A\u0942\u0930\u093E \u0928\
  \u093E\u092E \"YAML Ain't Markup Language\" \u0939\u0948, \u090F\u0915 \u092E\u093E\
  \u0928\u0935-\u092A\u0920\u0928\u0940\u092F \u0921\u0947\u091F\u093E \u0938\u0940\
  \u0930\u093F\u092F\u0932\u093E\u0907\u091C\u0947\u0936\u0928 \u092E\u093E\u0928\u0915\
  \ \u0939\u0948 \u091C\u094B \u0905\u0915\u094D\u0938\u0930 \u0915\u0949\u0928\u094D\
  \u092B\u093F\u0917\u0930\u0947\u0936\u0928 \u092B\u093E\u0907\u0932\u094B\u0902\
  \ \u0914\u0930 \u092D\u093E\u0937\u093E\u0913\u0902 \u0915\u0947 \u092C\u0940\u091A\
  \ \u0921\u0947\u091F\u093E \u0935\u093F\u0928\u093F\u092E\u092F \u0915\u0947\u2026"
lastmod: '2024-03-13T22:44:52.584795-06:00'
model: gpt-4-0125-preview
summary: "YAML, \u091C\u093F\u0938\u0915\u093E \u092A\u0942\u0930\u093E \u0928\u093E\
  \u092E \"YAML Ain't Markup Language\" \u0939\u0948, \u090F\u0915 \u092E\u093E\u0928\
  \u0935-\u092A\u0920\u0928\u0940\u092F \u0921\u0947\u091F\u093E \u0938\u0940\u0930\
  \u093F\u092F\u0932\u093E\u0907\u091C\u0947\u0936\u0928 \u092E\u093E\u0928\u0915\
  \ \u0939\u0948 \u091C\u094B \u0905\u0915\u094D\u0938\u0930 \u0915\u0949\u0928\u094D\
  \u092B\u093F\u0917\u0930\u0947\u0936\u0928 \u092B\u093E\u0907\u0932\u094B\u0902\
  \ \u0914\u0930 \u092D\u093E\u0937\u093E\u0913\u0902 \u0915\u0947 \u092C\u0940\u091A\
  \ \u0921\u0947\u091F\u093E \u0935\u093F\u0928\u093F\u092E\u092F \u0915\u0947\u2026"
title: "YAML \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\
  \u093E"
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
