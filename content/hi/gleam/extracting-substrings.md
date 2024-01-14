---
title:    "Gleam: स्ट्रिंग से सबस्ट्रिंग निकालना"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## क्यों

जैसा कि हम सभी जानते हैं, प्रोग्रामिंग में सॉफ्टवेयर डेवलपमेंट एक महत्वपूर्ण कौशल है जो हमें नए और रुचिकर तरीकों से प्रोग्राम लिखने को प्रेरित करता है। एक ऐसा तरीका है substring का प्रकार, जो प्रोग्रामिंग में अत्यंत उपयोगी है। लेकिन अब आपको इसका इस्तेमाल कैसे करना है, इसे एकदम समझने की आवश्यकता है। 

## कैसे करे

करने के लिए, हमें सबसे पहले एक डेटा टाइप बनाना होगा जो विशिष्ट स्ट्रिंग्स को स्टोर करेगा। उसके बाद हम ```Gleam.String.substr``` फ़ंक्शन का इस्तेमाल करके substring को निकाल सकते हैं। नीचे दिए गए कोड ब्लॉक में आपको दो उदाहरण दिए गए हैं जो substring को निकालने का तरीका दिखाते हैं।

```Gleam
// स्ट्रिंग डेटा टाइप बनाएं
pub type Person {
  name: String,
  age: Int,
  city: String
}

// सबसे पहले स्ट्रिंग वेरिएबल में एक विशिष्ट स्ट्रिंग स्टोर करें
let name = "John Doe"

// फ़ंक्शन के द्वारा सबसे पहले 5 चरों को निकालें
let substring = Gleam.String.substr(name, 0, 5)

// प्रिंट करे
pub fn main() {
  Gleam.IO.println(substring)
  // Output: "John"
}

// एक और उदाहरण
let city = "New York"

// फ़ंक्शन के द्वारा 4वें चर से शुरू करके 15वें चर तक निकालें
let substring = Gleam.String.substr(city, 4, 15)

// प्रिंट करे
pub fn main() {
  Gleam.IO.println(substring)
  // Output: "York"
}
```

## गहराई से जाने

Gleam में substring एक बहुत ही उपयोगी फ़ंक्शन है जो आपको स्ट्रिंग के चयनित हिस्से को निकालने में मदद करता है।