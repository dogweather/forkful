---
title:                "Gleam: यादृच्छिक संख्याएं उत्पन्न करना"
simple_title:         "यादृच्छिक संख्याएं उत्पन्न करना"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## क्यों

उपयोगकर्ता लोग या डेवलपर्स एक संदर्भ में स्थिर नंबरों का उत्पादन क्यों करना चाहेंगे? इसका सरल जवाब है - इससे हमारे कोड में तिरंगा दक्षता होगी।

## कैसे करें

यह लेख तरीकों को प्रदत्त करेगा कि आप जीवमान उपयोग में ले सकते हैं। निम्नलिखित उदाहरणों के साथ आप घर पर खुद की तिरंगा कोड देख सकते हैं । खासकर अंश " Hello" और सेशन को देखें। मूल्य गणना या ताजगी मापने से अरेरा और श्रृंखला सुस्पष्टतिकरण देरी पर एक समानता है।

```Gleam
pub enum Number {
  Integer(Int)
  Float(Float)
}
     
fn generate_random_number() {
  let number = 
          fn [] { Number.Integer(rand.int()) }
      }
  case number {
    Number.Integer(i)  -> Ok(i)
    Number.Float(i) -> Ok(i)
  }
}
```

```
```Gleam
pub enum Number {
  Integer(Int)
  Float(Float)
}
     
fn generate_random_number() {
  let number = 
          fn [] { Number.Float(rand.float()) }
      }
  case number {
    Number.Integer(i)  -> Ok(i)
    Number.Float(i) -> Ok(i)
  }
}
```

## गहराई से श्रवण

संदर्भ नंबरों का उत्पादन के बारे में संपर्क में तो हीर तर्क सूचनाएं सेट होता है जो कि स्वयं उम्मीदवार की लागत को क्षम करती है स्वाभाविक अवसाद का अवसर है। यह आवश्यक है कि इन स्वरूपों का उपयोग सार्वजनिक स्थिरता में सक्षम चैपरोज वर्चलोम गठन कर स्वाभाविक तरह से स्वचालित होगा।

## अधिक जानें

यहाँ छैपट्टा शीर्षक "देखें के "नाम लिखा जाता है और नीचे दिया गया है Hindi में एक सामग्री सूची के लिए किसी सामग्री संसाधक गाना पढ़ा। आगे के शब्द सूची आप म