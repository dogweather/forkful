---
title:                "स्ट्रिंग कैपिटलाइज करना।"
html_title:           "Java: स्ट्रिंग कैपिटलाइज करना।"
simple_title:         "स्ट्रिंग कैपिटलाइज करना।"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## क्या है और क्यों करें?

कैसे एक आदेश लगाएं?

आदेश लगाना एक कार्य है जहाँ हम एक स्ट्रिंग (टेक्स्ट का एक श्रृंखला) को प्रविष्ट करते हैं और उसके सभी शब्दों को प्रथम लेटर कैपिटल में बदल देते हैं। कई प्रोग्रामर्स इसको अपने टेक्स्ट प्रोग्राम में कस्टमाइज करने के लिए करते हैं।

## कैसे करें:

```Java
String name = "john doe";
name = name.substring(0,1).toUpperCase() + name.substring(1);
System.out.println(name);

// आउटपुट: John Doe
```

यहाँ हमने स्ट्रिंग को `substring()` और `toUpperCase()` फंक्शन का उपयोग करके प्रथम शब्द को कैपिटल में बदला है। आप इसमें किसी भी स्ट्रिंग को दाहिनी शब्द से कैपिटल में बदल सकते हैं।

## गहराई में:

पहले अप्रैल 1978 को PDP-10 कंप्यूटर पर एक क्लास के नियमों के साथ जारी किया गया था। अन्य विकल्प जैसे चर को विकल्पों के माध्यम से असाइन किया जा सकता है और स्ट्रिंग वर्तनी जैसे विकल्प भी हो सकते हैं। जैवैन और संयंजिकरण उन विकल्पों में से दो थे जो संयंजित श्रृंखला वर्तनी का उपयोग करते हैं।

## और भी देखें:

- [Java String class - Oracle documentation](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Java substring() method - W3Schools](https://www.w3schools.com/java/java_string_substring.asp)
- [Java toUpperCase() method - GeeksforGeeks](https://www.geeksforgeeks.org/java-lang-string-toupper-caseyaring/)