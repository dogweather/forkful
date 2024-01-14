---
title:                "Rust: यामल के साथ काम करना"
simple_title:         "यामल के साथ काम करना"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/working-with-yaml.md"
---

{{< edit_this_page >}}

## क्यों

Rust एक उन्नत और प्रभावी प्रोग्रामिंग भाषा है जो इनके सम्पर्क में आपको YAML को आसानी से हैंडल करने की अनुमति देती है। यह आपको पारस्परिक विश्लेषण और डेटा को अलग-अलग उपकरणों में आसानी से एकीकृत करने की सुविधा प्रदान करती है।

## कैसे करें

यूएमएल को रास्ट में हैंडल करने के लिए, सबसे पहले आपको serde के माध्यम से फ़ाइल या रेकॉर्ड को लोड करना होगा। ये समझने के लिए निम्नलिखित उदाहरण देखें:

```
use serde_yaml::Value;

fn main() {

   // फाइल से डेटा लोड करें
   let file = std::fs::File::open("data.yaml").expect("File not found");
   let data: Value = serde_yaml::from_reader(file).expect("Error while reading file");

   // डेटा प्रिंट करें
   println!("The data is: {:?}", data);

}
```

इसके बाद, आप किसी भी YAML डाटा को आसानी से लेने या सेट कर सकते हैं। आप भी अन्य विशिष्ट विश्लेषण और समस्याओं को हल करने के लिए यूएमएल डाटा से उपयोगी जानकारी प्राप्त कर सकते हैं।

```
// विशिष्ट डाटा से जानकारी प्राप्त करें
let username = &data["user"]["name"];
println!("The username is: {:?}", username);

// कोई खाली स्कलर मान हो तो उसके स्ट्रिंग पर गणितीय कार्य चलाएं
let department = data["user"]["department"].as_str().map_or("", |d| d.to_uppercase());
println!("The department is: {}", department);
```

## डीप डाइव

YAML एक लाइटवेट डाटा फॉर्मेट है जो हमें डेटा को हियरार्कीग्ली और स्थायी ढंग से हैंडल करने की अनुमति देता है। यह एक लिखा गया फॉरमेट है जो हमें अपने कोड को और अधिक सुविधाजनक बनाने में मदद कर सकता ह