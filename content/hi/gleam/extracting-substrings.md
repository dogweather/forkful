---
title:                "सबस्ट्रिंग्स निकालना"
date:                  2024-01-20T17:45:50.581344-07:00
model:                 gpt-4-1106-preview
simple_title:         "सबस्ट्रिंग्स निकालना"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
Substring निकालना मतलब किसी भी स्ट्रिंग का कोई छोटा हिस्सा चुनना। प्रोग्रामर्स ऐसा तब करते हैं जब उन्हें बड़ी स्ट्रिंग में से विशिष्ट जानकारी निकालनी होती है - जैसे यूजरनेम या डोमेन नेम ईमेल से।

## How to: (कैसे करें:)
```gleam
import gleam/substring.{from_to, from, to}

fn main() {
  let text = "नमस्ते दुनिया"
  
  // शुरुआत से स्थिति तक
  let hello = from_to(text, 0, 5)
  assert from_to(text, 0, 5) == Ok("नमस्ते") 
  
  // शुरुआत से निकालें
  let greeting = from(text, 0)
  assert from(text, 0) == Ok("नमस्ते दुनिया")
  
  // स्थिति से अंत तक
  let world = to(text, 7)
  assert to(text, 6) == Ok("दुनिया")
  
  // उदाहरण के आउटपुट्स
  case hello {
    Ok(substring) -> io.println(substring)
    Error(_) -> io.println("Error in extracting substring")
  }
  
  case world {
    Ok(substring) -> io.println(substring)
    Error(_) -> io.println("Error in extracting substring")
  }
}
```
सैंपल आउटपुट:
```
नमस्ते
दुनिया
```

## Deep Dive (गहराई से जांच)
Substring निकालने की जरूरत पुराने जमाने से चली आ रही है, जब पहली बार टेक्स्ट प्रोसेसिंग शुरू हुई थी। विभिन्न भाषाओं में अलग-अलग तरीके से सबस्ट्रिंग निकाली जाती है - कुछ index-based हैं, कुछ pattern-based हैं। Gleam में, `from_to`, `from`, और `to` फंक्शन्स substring को सही और सीधे तरीके से निकालने में मदद करते हैं। Error handling भी इन-बिल्ट होती है जो इंडेक्स-संबंधित गलतियों को संभालती है।

## See Also (और जानकारी के लिए)
- प्रोग्रामिंग में स्ट्रिंग्स से जुड़े कॉन्सेप्ट्स: [String Concept in Programming](https://www.geeksforgeeks.org/string-data-structure/)
