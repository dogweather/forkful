---
title:    "Arduino: स्ट्रिंग को कैपिटलाइज करना"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

# यहाँ कोडिंग क्यों प्रोग्रामिंग में महत्वपूर्ण है?

क्या आपने कभी सोचा है कि जब हम किसी प्रोग्राम में दिए गए शब्दों को बड़े अक्षरों में लिखते हैं, तो उसमें कोई खास कारण हो सकता है? ध्यान से समझने पर आपको यह पता चलेगा कि बड़े अक्षरों में लिखा गया शब्द आमतौर पर किसी विशेष अर्थ को संकेत करता है। आइये जानते हैं कि आर्डुइनो प्रोग्रामिंग में शब्दों को बड़े अक्षरों में लिखने के लिए हम क्या कोडिंग कर सकते हैं।

## कैसे करें: शब्दों को बड़े अक्षरों में कैपिटलाइज करना

```
Arduino void setup () {
  Serial.begin (9600);
  String word = "hello";
  Serial.println(capitalizeString(word));
}

String capitalizeString (String str) {
    for (int i = 0; i < str.length (); i++) {
        str.setCharAt (i, toupper (str.charAt (i)));
    }
    return str;
}
```

```
Hindi:
Arduino void setup () {
  Serial.begin (9600);
  String word = "नमस्ते";
  Serial.println(capitalizeString(word));
}

String capitalizeString (String str) {
    for (int i = 0; i < str.length (); i++) {
        str.setCharAt (i, toupper (str.charAt (i)));
    }
    return str;
}
```

ऊपर दिए गए कोड में हमने स्ट्रिंग वेरिएबल को बड़े अक्षरों में वापस देने के लिए `capitalizeString` फंक्शन बनाया है। `for` लूप के माध्यम से हमने हर अक्षर को उच्च अक्षर में बदल दिया है। आप `capitalizeString` फंक्शन को अपने कोड में इस्तेमाल कर सकते हैं और अपने शब्दों को बड़े अक्षरों में बदल सकते हैं। यह आपके बड़े अक्षरों में लिखे गए शब्दों को और उपयोगी बनाता है।

## गहराई तक: बड़े अक्षरों में शब्द लिख्ने का अधिक जानकार