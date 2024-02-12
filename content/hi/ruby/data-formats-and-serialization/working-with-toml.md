---
title:                "TOML के साथ काम करना"
aliases:
- hi/ruby/working-with-toml.md
date:                  2024-01-26T04:26:51.753561-07:00
model:                 gpt-4-0125-preview
simple_title:         "TOML के साथ काम करना"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/working-with-toml.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

TOML एक कॉन्फ़िग फ़ाइल फ़ॉर्मेट है जिसे इसकी स्पष्ट सिमैंटिक्स के कारण पढ़ना आसान होता है। प्रोग्रामर्स TOML का उपयोग ऐप कॉन्फ़िग्स और डेटा सीरियलाइजेशन को प्रबंधित करने के लिए करते हैं, XML की भारतांकन या YAML की विचित्रताओं के बिना।

## कैसे करें:

पहले, `toml-rb` गेम को इंस्टॉल करें। यह Ruby में TOML पार्सिंग के लिए एक लोकप्रिय विकल्प है।

```Ruby
gem install toml-rb
```

अगला, TOML फ़ाइल को पढ़ना:

```Ruby
require 'toml-rb'

toml_content = File.read('config.toml')
config = TomlRB.parse(toml_content)
puts config['title']
```

नमूना आउटपुट हो सकता है:

```
My Awesome App
```

एक TOML फ़ाइल में लिखना:

```Ruby
require 'toml-rb'

config = {
  'title' => 'My Awesome App',
  'owner' => {
    'name' => 'John Doe',
    'dob' => Date.new(1979, 5, 27)
  }
}

toml_string = TomlRB.dump(config)
File.write('config.toml', toml_string)
```

`config.toml` को चैक करें और आप देखेंगे कि आपकी सेटिंग्स नीटली स्टोर की गई हैं।

## गहरी गोता

TOML, जिसका अर्थ है Tom's Obvious, Minimal Language, लगभग 2013 के आसपास GitHub के सह-संस्थापक टॉम प्रेस्टन-वर्नर द्वारा बनाई गई थी। इसका प्राथमिक उद्देश्य एक सरल फॉरमेट होना है जिसे डेटा संरचनाओं में आसानी से पार्स किया जा सके। जबकि JSON APIs के लिए बढ़िया है, और YAML लचीला है, TOML की खासियत इसका मनुष्य-मित्रता पर जोर है। YAML के विपरीत, जो इंडेंटेशन के साथ छिद्र खोजने में सक्षम हो सकता है, TOML एक अधिक INI-जैसी संरचना की ओर लक्षित है जिसे कई लोग सरल और कम गलती-प्रोन पाते हैं।

JSON, YAML, या XML जैसे विकल्पों में प्रत्येक के अपने स्वयं के बल हैं, लेकिन TOML उन परिदृश्यों में सफल होता है जहां एक कॉन्फ़िग को मनुष्यों और कार्यक्रमों द्वारा समान रूप से आसानी से बनाए रखा जाना चाहिए। यह न केवल सरल है, बल्कि कड़ाई से पढ़ने योग्य फ़ॉर्मेटिंग को लागू करता है।

तकनीकी पक्ष पर, Ruby के साथ TOML सामग्री को पार्स करने के लिए, हम `toml-rb` जैसे गेम्स का लाभ उठाते हैं। यह गेम Ruby की गतिशील प्रकृति का लाभ उठाता है, TOML डेटा को मूल Ruby हैशेज, ऐरे, और अन्य मूल डेटा संरचनाओं में बदलता है। यह परिवर्तन का अर्थ है कि डेवलपर्स TOML डेटा के साथ परिचित Ruby सेमेंटिक्स और विधियों का उपयोग करके काम कर सकते हैं।

## देखें भी

- TOML परियोजना और विशिष्टता: https://toml.io/en/
- `toml-rb` गेम: https://github.com/emancu/toml-rb
- TOML, YAML, और JSON की तुलना: https://blog.theodo.com/2021/08/compare-yml-toml-json/
