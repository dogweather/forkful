---
title:                "कमांड लाइन तर्कों को पढ़ना"
html_title:           "Kotlin: कमांड लाइन तर्कों को पढ़ना"
simple_title:         "कमांड लाइन तर्कों को पढ़ना"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## क्या और क्यों? ("What & Why?")

कमांड लाइन आर्गुमेंट्स पढ़ना एक प्रक्रिया होती है जिसके द्वारा प्रोग्राममेर्स अपने प्रोग्राम्स को कस्टमाइज़ कर पाते हैं जब उन्हें रन किया जाता है। यह उन्हें गतिशीलता देता है और हार्डकोडिंग आवश्यकताओं को कम करता है।

## कैसे: ("How to:")

यहाँ एक आसान Java कोड है जो कमांड लाइन आर्गुमेंट्स को पढ़ता है:

```Java
public class CommandLineArgs {
    public static void main(String[] args) {
        for(String arg: args) {
            System.out.println(arg);
        }
    }
}
```

आप इसे इस तरह से रन कर सकते हैं, जैसे की:

```
> java CommandLineArgs यहाँ मेरा पाठ है
```

और आपको आउटपुट इस तरह का मिलेगा :

```
यहाँ
मेरा
पाठ
है
```

## गहराई में: ("Deep Dive")

1. **ऐतिहासिक प्रसंग:** उपयोगकर्ता-निर्दिष्ट आर्गुमेंट्स का कमांड लाइन पर आधारित प्रोग्रामों में काफी समय से इस्तेमाल हो रहा है, और यह Java के साथ भी आ गया था।

2. **विकल्प:** अगर आपके प्रोग्राम को आवश्यकता है उपयोगकर्ता प्रदत्त इनपुट की, तो आप `Scanner` क्लास भी उपयोग कर सकते हैं, लेकिन इससे सीधे तकरीबन सभी कमांड-लाइन आर्गुमेंट्स पर काम नहीं किया जा सकता।

3. **कार्यान्वयन विवरण:** `args` वास्तव में एक स्ट्रिंग ऐरे होता है जिसमें कमांड-लाइन पर पाठित आर्गुमेंट्स के अलावा और कुछ नहीं होता।

## देखें भी: ("See Also")
1. Oracle Docs: [Command-Line Arguments](https://docs.oracle.com/en/java/javase/17/docs/specs/man/java.html)