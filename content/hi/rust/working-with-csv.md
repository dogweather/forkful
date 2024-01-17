---
title:                "csv के साथ काम करना"
html_title:           "Rust: csv के साथ काम करना"
simple_title:         "csv के साथ काम करना"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/working-with-csv.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
CSV काम करना क्या है और क्यों प्रोग्रामर्स इसका इस्तेमाल करते हैं, यह जानना महत्वपूर्ण है। CSV एक साधारण फ़ाइल फ़ॉर्मेट है जो अलग-अलग डेटा प्रकारों को एक साथ शामिल करता है। इससे प्रोग्रामर्स को जटिल डेटा को संगठित और प्रक्रियात्मक दोनों बनाने में मदद मिलती है।

## कैसे:
```Rust
use std::fs::File;
use std::error::Error;
use csv::Reader;

fn main() -> Result<(), Box<dyn Error>> {
  // CSV फ़ाइल लोड करें
  let file = File::open("data.csv")?;
  let mut reader = Reader::from_reader(file);

  // प्रत्येक पंक्ति के लिए डेटा प्रिंट करें
  for result in reader.records() {
    let record = result?;
    println!("{:?}", record);
  }

  Ok(())
}
```

सामान्यतया, CSV लाइब्रेरी प्रोग्रामर्स को CSV फ़ाइल के माध्यम से डेटा को पढ़ने, लिखने और अन्य कार्रवाई करने की सुविधा प्रदान करती है। इस उदाहरण में, हमने फ़ाइल से डेटा पढ़ा और प्रत्येक पंक्ति को प्रिंट किया है। 

## गहराई में जाएँ:
CSV 1987 में पेश किया गया था और इसे कॉमा से अलग करने के लिए बनाया गया था। यह उपलब्धता के साथ बहुत ही प्रचलित बन गया है और अन्य फ़ॉर्मेट के मुकाबले सरल है। अन्य विकल्प में XML और JSON शामिल हैं, लेकिन वे अधिक जटिल हैं। CSV फ़ाइलें साधारण फ़ाइल फ़ॉर्मेट होने के कारण, यह विभिन्न भाषाओं में आसानी से समर्थित होती हैं।

CSV फ़ाइलों को रीड करने के लिए कई अलग-अलग लाइब्रेरी हैं, जैसे कि `csv`, `csv_crate` और `rscsv`। इन में से ध्यान देने योग्य है कि कुछ लाइब्रेरी स्ट्रिंग या विशिष्ट डेटा टाइप को समर्थित करती हैं, जबकि कुछ `serde` जैसी लाइब्रेरी को एक्स्पोर्ट करती हैं। आपको अपने आवश्यकतानुसार लाइब्रेरी का चयन करना होगा।

## इसके साथ देखें:
- [Rust CSV दस्तावेज़](https://docs.rs/csv)
- [Rust के साथ CSV कैसे काम करें](https://www.youtube.com/watch?v=hgdRKQfz8f8&ab_channel=TheRustProgrammingLanguage)