---
title:                "स्टैंडर्ड त्रुटि पर लिखना"
html_title:           "Java: स्टैंडर्ड त्रुटि पर लिखना"
simple_title:         "स्टैंडर्ड त्रुटि पर लिखना"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

जावा में, standard error से लिखना यह है कि हम किसी भी त्रुटि या अनुमति को प्रोग्राम को सुधारने के लिए पूरक रूप से और काफी उपयोगी रूप से सुचना देते हैं। यह पूरी तरह से त्रुटि-सुधार आधारित है और प्रोग्रामिंग को स्पष्टता से बनाने में मदद करता है। प्रोग्रामर्स standard error का उपयोग करते हैं क्योंकि यह खास त्रुटि सूचनाएं देता है जो प्रोग्राम में अमान्य या दोषपूर्ण कोड को पकड़ने में सहायता करती है।

## कैसे करें:

```
// एक standard error से लिखनेवाली एक कलम
System.err.println("यह एक त्रुटि है।");

// यहां एक उदाहरण है जहां हम कोई पेशेवर नंबर (professional number) नहीं प्राप्त कर पाए
try {
  int professionalNumber = Integer.parseInt(input); // उपयोगकर्ता द्वारा हुई इनपुट
} catch (NumberFormatException e) {
  System.err.println("एक अवैध पेशेवर नंबर है।");
}
```

```
यह उदाहरण प्रोग्राम के output है:
अगर उपयोगकर्ता 1999999 को प्रविष्ट करता है तो यह उत्पाद आउटपुट होगा:
यह एक अवैध पेशेवर नंबर है।
```

## गहराई में जाएं:

पहले से ही, standard error Unix सिस्टम्स का हिस्सा है। इसका प्रयोग करें और अपने प्रोग्राम में अपनी त्रुटियों को पकड़ें। लेकिन, आप सीख सकते हैं कि कैसे standard output (कांसोल) और standard error का उपयोग करके अपने प्रोग्रामों में स्पष्टता जोड़ सकते हैं। इसके अलावा, आप alternative methods की भी बात कर सकते हैं जो errors को कैसे handle करते हैं, जैसे try-catch ब्लॉक या exception handling के साथ।

## और देखें:

- [Java Docs: Standard Error](https://docs.oracle.com/javase/7/docs/api/java/lang/System.html#err)
- [Exception Handling in Java](https://www.geeksforgeeks.org/exception-handling-java/)