---
title:                "पैटर्न से मिलते जुलते वर्णों को हटाना"
html_title:           "Elixir: पैटर्न से मिलते जुलते वर्णों को हटाना"
simple_title:         "पैटर्न से मिलते जुलते वर्णों को हटाना"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

"मिलाने वाले वर्णों को हटाना" एक क्रिया है जिसमें एक प्रोग्रामिंग भाषा का उपयोग करके आप एक विशिष्ट पैटर्न से मेल खाने वाले वर्णों को हटा सकते हैं। कार्यक्रमकर्ता इसे पाठ संसाधनों को मुख्य रूप से मान्यता प्राप्त करने और अनावश्यक उपादानों को दूर करने के लिए करते हैं। 

## कैसे करें:

Elm में, `String.foldl` और `String.contains` की मदद से आप इस कार्य को संभवनीय रूप से कर सकते हैं।

```Elm
patternDelete : String -> String -> String
patternDelete pattern string =
    String.foldl (\char output -> 
      if String.contains (String.fromChar char) pattern then 
        output
      else 
        String.append output (String.fromChar char)) "" string
```

ऊपर दिए गए कोड का उदाहरण देखने के लिए:

```Elm
patternDelete "aeiou" "Hello, World"
```

आउटपुट:

```Elm
"Hll, Wrld"
```

## गहरी डाइव

इस फंक्शन की सामर्थ्य साधारण रूप से वाइल्डकार्ड और पैटर्न मिलान की भाषाओं में प्रेषित होती है और यदादृच्छिक पाठ प्रणाली के द्वारा सुनिश्चित की जाती है। संख्यात्मक और कम्प्यूटेशनल क्षेत्रों में अक्सर अनावश्यक वर्णों को निकालने का इस तरीके का उपयोग किया जाता है। Elm में इसे `String.foldl` और `String.contains` से सुविधाजनक रूप से लागू किया जा सकता है।

## यह भी देखें:

नीचे दिए गए कुछ साधारण और उन्नत टुटोरियल्स और स्रोतों पर जाएं जो आपको और अधिक सीखने में मदद करेंगे:

3. [Elm और pattern matching पर डीप डाइव](https://elmprogramming.com/pattern-matching.html) 

ध्यान दें कि Elm भाषा शुद्ध कार्यों और असिंक्रोन प्रोग्रामिंग के बीच अद्वितीय संतुलन की सिफारिश करती है, जिससे कार्यक्रमकर्ताओं को सुरक्षित और सटीक कोड लिखने में मदद मिलती है।