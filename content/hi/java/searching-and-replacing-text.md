---
title:                "Java: कंप्यूटर प्रोग्रामिंग पर 'टेक्स्ट खोज व प्रतिस्थापन'."
simple_title:         "कंप्यूटर प्रोग्रामिंग पर 'टेक्स्ट खोज व प्रतिस्थापन'."
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

क्यों: कई बार हम अपनी पाठ को बदलना चाहते हैं, जैसे कि डोक्यूमेंट या कोड में शब्दों को बदलना। इसलिए हमारे पास सरल तरीके की जरूरत होती है जो हमें उस बदलाव से बचाए रखते हैं। 

कैसे करें: नीचे दिए गए उदाहरणों और कोड ब्लॉक में दिखाए गए आउटपुट के साथ हम बताएंगे कि हम टेक्स्ट को कैसे खोजें और बदलें। यह स्टेप-बाइ-स्टेप गाइड आपको बताएगा कि आप अपने कोड को कैसे लिख सकते हैं जो आपको सरल तरीके से टेक्स्ट को ढूंढने और बदलने में मदद करेगा।

```Java

// टेक्स्ट में शब्द ढूंढें और बदलें
String text = "मेरा नाम अमित है।";
String newText = text.replace("अमित", "रोहित");
System.out.println(newText);
// आउटपुट: मेरा नाम रोहित है।

// ऑनस्क्रीन आंकड़े छोटे करें
String number = "1,000";
String newNumber = number.replace(",", "");
int intNumber = Integer.parseInt(newNumber);
System.out.println(intNumber);
// आउटपुट: 1000

// बड़े आंकड़ों से लेखक को ढूंढें और उसे बदलें
String text = "लेखक: अमित";
int startPos = text.indexOf(":") + 2; // ": " के बाद की शुरुआत को ढूंढें
String newText = "लेखक: रोहित";
String replacedText = text.substring(0, startPos) + newText;
System.out.println(replacedText);
// आउटपुट: लेखक: रोहित

```

डीप डाइव: शब्दों को खोजने और बदलने के आगे दिए गए कोड की गहराई अहम है। समझने के लिए, आपको यह सुनिश्चित करना होगा कि आपको सही तरीके से शब्दों को खोजने और बदलने के तरीके को जानना है। आप अपने खाली गुणांक से