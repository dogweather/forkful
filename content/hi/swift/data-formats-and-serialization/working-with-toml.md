---
date: 2024-01-26 04:27:42.821546-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: \u0936\u0941\u0930\
  \u0942 \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F, \u0906\u092A\u0915\
  \u094B \u090F\u0915 TOML \u092A\u093E\u0930\u094D\u0938\u0930 \u0915\u0940 \u0906\
  \u0935\u0936\u094D\u092F\u0915\u0924\u093E \u0939\u094B\u0924\u0940 \u0939\u0948\
  \u0964 \u0938\u094D\u0935\u093F\u092B\u094D\u091F \u092E\u0947\u0902 \u090F\u0915\
  \ \u092C\u093F\u0932\u094D\u091F-\u0907\u0928 \u0928\u0939\u0940\u0902 \u0939\u0948\
  , \u0907\u0938\u0932\u093F\u090F \u091A\u0932\u093F\u090F `TOMLDecoder` \u0915\u093E\
  \ \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0947\u0902\u0964 \u0907\u0938\u0947\
  \ \u0938\u094D\u0935\u093F\u092B\u094D\u091F\u2026"
lastmod: '2024-03-13T22:44:52.958498-06:00'
model: gpt-4-0125-preview
summary: "\u0936\u0941\u0930\u0942 \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\
  \u090F, \u0906\u092A\u0915\u094B \u090F\u0915 TOML \u092A\u093E\u0930\u094D\u0938\
  \u0930 \u0915\u0940 \u0906\u0935\u0936\u094D\u092F\u0915\u0924\u093E \u0939\u094B\
  \u0924\u0940 \u0939\u0948\u0964 \u0938\u094D\u0935\u093F\u092B\u094D\u091F \u092E\
  \u0947\u0902 \u090F\u0915 \u092C\u093F\u0932\u094D\u091F-\u0907\u0928 \u0928\u0939\
  \u0940\u0902 \u0939\u0948, \u0907\u0938\u0932\u093F\u090F \u091A\u0932\u093F\u090F\
  \ `TOMLDecoder` \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0947\u0902\
  \u0964 \u0907\u0938\u0947 \u0938\u094D\u0935\u093F\u092B\u094D\u091F \u092A\u0948\
  \u0915\u0947\u091C \u092E\u0948\u0928\u0947\u091C\u0930 \u0915\u0947 \u092E\u093E\
  \u0927\u094D\u092F\u092E \u0938\u0947 \u0907\u0902\u0938\u094D\u091F\u0949\u0932\
  \ \u0915\u0930\u0947\u0902 \u0914\u0930 \u092B\u093F\u0930 TOML \u0915\u094B \u0906\
  \u0938\u093E\u0928\u0940 \u0938\u0947 \u0938\u0940\u0930\u093F\u092F\u0932\u093E\
  \u0907\u091C \u0914\u0930 \u0921\u093F\u0938\u0940\u0930\u093F\u092F\u0932\u093E\
  \u0907\u091C \u0915\u0930\u0947\u0902\u0964."
title: "TOML \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\
  \u093E"
weight: 39
---

## कैसे करें:
शुरू करने के लिए, आपको एक TOML पार्सर की आवश्यकता होती है। स्विफ्ट में एक बिल्ट-इन नहीं है, इसलिए चलिए `TOMLDecoder` का उपयोग करें। इसे स्विफ्ट पैकेज मैनेजर के माध्यम से इंस्टॉल करें और फिर TOML को आसानी से सीरियलाइज और डिसीरियलाइज करें।

```Swift
import TOMLDecoder

let tomlString = """
title = "TOML Example"

[owner]
name = "Tom Preston-Werner"
dob = 1979-05-27T07:32:00Z
"""

struct Config: Codable {
    let title: String
    let owner: Owner
}

struct Owner: Codable {
    let name: String
    let dob: Date
}

let decoder = TOMLDecoder()
if let configData = tomlString.data(using: .utf8) {
    do {
        let config = try decoder.decode(Config.self, from: configData)
        print("टाइटल: \(config.title), मालिक: \(config.owner.name), जन्म तारीख: \(config.owner.dob)")
    } catch {
        print("TOML पार्सिंग में गलती: \(error)")
    }
}
```

इस कोड का आउटपुट:
```
टाइटल: TOML Example, मालिक: Tom Preston-Werner, जन्म तारीख: 1979-05-27 07:32:00 +0000
```

## गहन अध्ययन
TOML को टॉम प्रेस्टन-वर्नर, गिटहब के सह-संस्थापक ने जेसन या YAML जैसे प्रारूपों के मुकाबले अधिक मानव-हितकारी विकल्प के रूप में डिजाइन किया था। इसका उद्देश्य स्पष्टता है, जिससे कि एक मानव या मशीन द्वारा गलत व्याख्या की संभावनाएं कम होती हैं। विकल्पों के रूप में, YAML और JSON सामान्य संदेही हैं, जहाँ YAML मानव पठनीयता की ओर झुका है और JSON सरल मशीन-अनुकूल विकल्प के रूप में है। स्विफ्ट में TOML के साथ काम करते समय, हमारे पास एक मूल पार्सर नहीं है। हालांकि, `TOMLDecoder` जैसे थर्ड-पार्टी लाइब्रेरीज TOML स्ट्रिंग्स और स्विफ्ट प्रकारों के बीच आसान रूपांतरण की सुविधा प्रदान करते हैं, विशेष रूप से स्विफ्ट 4 में परिचय दिए गए `Codable` प्रोटोकॉल के माध्यम से जो सीरियलाइजेशन को स्ट्रीमलाइन करता है।

## देखें भी
- TOML मानक: https://toml.io
- `TOMLDecoder` के लिए गिटहब: https://github.com/dduan/TOMLDecoder
- `Codable` पर स्विफ्ट दस्तावेज़ीकरण: https://developer.apple.com/documentation/swift/codable
- डाटा सीरियलाइजेशन प्रारूपों की तुलना: https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats
