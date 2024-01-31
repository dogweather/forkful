---
title:                "TOML के साथ काम करना"
date:                  2024-01-26T04:23:40.646574-07:00
model:                 gpt-4-0125-preview
simple_title:         "TOML के साथ काम करना"

category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/working-with-toml.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
TOML के साथ काम करने का अर्थ है TOML (Tom's Obvious, Minimal Language) फाइलों को पार्स करना और एन्कोड करना Go में। प्रोग्रामर्स TOML का चयन इसकी पठनीयता और डेटा संरचनाओं के साथ आसान मैपिंग के कारण करते हैं, जो कि कॉन्फ़िग्स के लिए एक मजबूत विकल्प है।

## कैसे:
Go में TOML के साथ काम करने के लिए, आप आम तौर पर `BurntSushi/toml` जैसे लाइब्रेरी का उपयोग करेंगे। यहां एक TOML कॉन्फ़िग फ़ाइल को पार्स करने का एक त्वरित दृष्टिकोण है:

```Go
package main

import (
    "fmt"
    "os"

    "github.com/BurntSushi/toml"
)

type Config struct {
    Title   string
    Owner   struct {
        Name string
    }
}

func main() {
    var config Config
    if _, err := toml.DecodeFile("config.toml", &config); err != nil {
        fmt.Println(err)
        return
    }
    fmt.Printf("Title: %s, Owner: %s\n", config.Title, config.Owner.Name)
}
```

`config.toml` का नमूना:

```Toml
title = "Example TOML"
[owner]
name = "Tom Preston-Werner"
```

नमूना आउटपुट:

```
Title: Example TOML, Owner: Tom Preston-Werner
```

## गहराई में जानें
TOML, जिसे Tom Preston-Werner ने 2013 में पेश किया था, एक सरल कॉन्फ़िगरेशन फाइल फॉर्मेट के रूप में डिज़ाइन किया गया था जिसे इसके स्पष्ट सिमेंटिक्स के कारण पढ़ना आसान है। Go डेवलपर्स अक्सर JSON या YAML जैसे विकल्पों के ऊपर TOML का उपयोग कॉन्फ़िगरेशन के लिए करते हैं क्योंकि इसकी सादगी और जटिल हायरार्की को सरलता से प्रस्तुत करने की क्षमता के लिए।

YAML की तुलना में, जिसमें जटिल सुविधाएँ और संभावित सुरक्षा चिंताएँ हैं, TOML का सपाट डिजाइन जटिलता और टाइपो-जनित त्रुटियों को कम करता है। और JSON के विपरीत, TOML में कमेंट्स का समर्थन होता है, जिससे कॉन्फ़िगरेशन को लाइन-में समझाना आसान होता है।

Go में TOML के साथ काम करते समय, विचार करने के लिए विशेषताएं होती हैं। स्ट्रक्ट टैग्स यह नियंत्रित कर सकते हैं कि आपके स्ट्रक्चर्स TOML संरचनाओं से कैसे मैप होते हैं, और आपको यह भी पता होना चाहिए कि Go स्लाइस और मैप्स में TOML ऐरेज़ और इनलाइन टेबल्स कैसे पार्स होते हैं।

## देखें भी
- TOML विशिष्टता: https://toml.io/en/
- BurntSushi/toml लाइब्रेरी: https://github.com/BurntSushi/toml
- कॉन्फ़िग फ़ाइल फॉर्मेट्स की तुलना: https://www.redhat.com/sysadmin/yaml-toml-json-differences
