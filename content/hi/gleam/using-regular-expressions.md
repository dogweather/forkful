---
title:                "रेगुलर एक्सप्रेशन का उपयोग"
html_title:           "Bash: रेगुलर एक्सप्रेशन का उपयोग"
simple_title:         "रेगुलर एक्सप्रेशन का उपयोग"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/using-regular-expressions.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

रेगुलर एक्सप्रेशंस टेक्स्ट पैटर्न्स को ढूंढने के लिए होते हैं। प्रोग्रामर्स इनका इस्तेमाल डेटा वैलिडेशन, खोज और टेक्स्ट मॉडिफिकेशन के लिए करते हैं।

## कैसे करें:

यहाँ Gleam में रेगुलर एक्सप्रेशंस का उदाहरण दिया गया है:

```Gleam
import gleam/regex

fn main() {
  // एक सिंपल रेगुलर एक्सप्रेशन पैटर्न
  let pattern = regex.compile("\d+").unwrap()
  // पैटर्न को मैच करने के लिए एक टेक्स्ट
  let text = "इस संख्या 1234 को देखें"
  // पैटर्न मैच करना
  let matches = pattern.find(text)
  
  case matches {
    Ok(found) -> 
      found |> List.map(fn(x) { x.group(0) })
    Error(_) -> 
      []
  }
}
```

उपरोक्त कोड से आउटपुट ऐसा आएगा: 

```
["1234"]
```

## गहराई से:

रेगुलर एक्सप्रेशंस का आविष्कार 1950 के दशक में हुआ था। इसके विकल्प में टेक्स्ट प्रोसेसिंग लाइब्रेरीज, पार्सर जेनरेटर्स और मेन्युअल स्ट्रिंग मैनिप्युलेशन का इस्तेमाल होता है। ग्लिम (Gleam) रेगुलर एक्सप्रेशन्स को एर्लांग इकोसिस्टम से लेकर एफिशिएंट परफॉरमेंस के लिए इस्तेमाल करता है।

## इसके अलावा:

- Gleam दस्तावेज: [https://gleam.run/book](https://gleam.run/book)
- रेगुलर एक्सप्रेशंस की बुनियादी जानकारी: [https://www.regular-expressions.info](https://www.regular-expressions.info)
- एर्लांग रेगुलर एक्सप्रेशंस (re module): [http://erlang.org/doc/man/re.html](http://erlang.org/doc/man/re.html)
