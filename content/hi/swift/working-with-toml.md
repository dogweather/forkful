---
title:                "TOML के साथ काम करना"
date:                  2024-01-26T04:27:42.821546-07:00
model:                 gpt-4-0125-preview
simple_title:         "TOML के साथ काम करना"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/working-with-toml.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
TOML (टॉम्स ऑब्वियस, मिनिमल लैंग्वेज) एक डाटा सीरियलाइजेशन प्रारूप है जो इसके स्पष्ट सिमेंटिक्स की वजह से पढ़ने में आसान होता है। प्रोग्रामर्स TOML का इस्तेमाल कॉन्फ़िगरेशन फ़ाइलों के लिए करते हैं जहाँ मानवों द्वारा पढ़ने और मशीनों द्वारा आसानी से पार्सिंग की जरूरत होती है।

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
