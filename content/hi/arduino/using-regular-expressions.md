---
title:                "रेगुलर एक्सप्रेशन का उपयोग"
html_title:           "Bash: रेगुलर एक्सप्रेशन का उपयोग"
simple_title:         "रेगुलर एक्सप्रेशन का उपयोग"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)

रेग्युलर एक्सप्रेशन्स यानि नियमित अभिव्यक्तियाँ पैटर्न मैचिंग के लिए इस्तेमाल होती हैं। ये प्रोग्रामर्स को टेक्स्ट खोजने, बदलने और मैनेज करने में मदद करती हैं।

## कैसे करें? (How to:)

Arduino में रेग्युलर एक्सप्रेशंस का सीधा समर्थन नहीं है, पर हम स्ट्रिंग फंक्शन्स से समान काम कर सकते हैं। उदाहरण:

```arduino
String inputData = "Temp=25*C";
int tempIndex = inputData.indexOf("Temp=");
if(tempIndex != -1) {
  int valueStart = tempIndex + 5;
  String tempValue = inputData.substring(valueStart, valueStart + 2);
  Serial.println(tempValue); // 25
}
```

## गहराई से जानकारी (Deep Dive)

Arduino पर रेग्युलर एक्सप्रेशंस का इतिहास कम है क्योंकि यह हल्के-फुल्के माइक्रोकंट्रोलर्स के लिए बानी भाषा है। यदि आवश्यकता पड़ी, तो हम बाहरी लाइब्रेरीज़ का इस्तेमाल कर सकते हैं या फिर प्रोसेसिंग पावर वाले डिवाइस पर कर सकते हैं।

## यह भी देखें (See Also)

- Arduino String Reference: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- Arduino External Libraries: https://www.arduino.cc/en/Guide/Libraries