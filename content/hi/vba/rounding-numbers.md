---
title:                "संख्याओं को पूर्णकरण"
date:                  2024-02-01T22:02:42.341818-07:00
model:                 gpt-4-0125-preview
simple_title:         "संख्याओं को पूर्णकरण"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/vba/rounding-numbers.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

प्रोग्रामिंग में संख्याओं को गोल करना एक संख्या को उसके निकटतम पूर्णांक या निश्चित संख्या के दशमलव स्थानों तक सन्निहित करने के बारे में होता है। प्रोग्रामर संख्याओं को गोल करते हैं ताकि आंकड़ों को सरल बनाया जा सके, पठनीयता में सुधार हो, या गणनाओं में विशिष्ट संख्यात्मक मापदंडों को पूरा किया जा सके, विशेषकर वित्तीय गणनाओं में जहां सटीकता मायने रखती है।

## कैसे:

विजुअल बेसिक फ़ॉर एप्लिकेशंस (VBA) में, गोलाकार करने को कई फंक्शन का उपयोग करके हासिल किया जा सकता है, प्रत्येक विशिष्ट परिदृश्यों के लिए उपयुक्त है। यहाँ सबसे अधिक प्रयुक्त फंक्शन उदाहरणों के साथ दिए गए हैं:

1. **राउंड फंक्शन**:
   `Round` फंक्शन एक संख्या को निर्दिष्ट संख्या के अंकों तक गोल करता है।
   ```basic
   Dim roundedNumber As Double
   roundedNumber = Round(3.14159, 2)  ' आउटपुट: 3.14
   MsgBox roundedNumber
   ```
   
2. **इंट और फिक्स फंक्शन**:
   `Int` और `Fix` दोनों फंक्शन का उपयोग संख्याओं को सबसे निकट के पूर्णांक तक गोल करने के लिए किया जाता है, लेकिन नकारात्मक संख्याओं के साथ वे विभिन्न तरीके से व्यवहार करते हैं।
   ```basic
   Dim intRounded As Integer
   Dim fixRounded As Integer
   
   intRounded = Int(-3.14159)  ' आउटपुट: -4
   fixRounded = Fix(-3.14159)  ' आउटपुट: -3
   
   MsgBox "Int: " & intRounded & ", Fix: " & fixRounded
   ```

3. **सीलिंग और फ्लोर फंक्शन**:
   VBA में अन्य भाषाओं में पाए जाने वाले निर्मित `Ceiling` और `Floor` फंक्शन का अभाव है। इसका समाधान करने के लिए, एक्सेल VBA के लिए `Application.WorksheetFunction.Ceiling_Math` और `Application.WorksheetFunction.Floor_Math` का उपयोग करें।
   ```basic
   Dim ceilingNumber As Double
   Dim floorNumber As Double
   
   ceilingNumber = Application.WorksheetFunction.Ceiling_Math(3.14159)  ' आउटपुट: 4
   floorNumber = Application.WorksheetFunction.Floor_Math(3.14159)  ' आउटपुट: 3
   
   MsgBox "Ceiling: " & ceilingNumber & ", Floor: " & floorNumber
   ```

## गहन अन्वेषण

VBA में `Round` फंक्शन अन्य भाषाओं में गोलाकार मेथडों से मूल रूप से भिन्न होता है क्योंकि इसका उपयोग **बैंकर्स राउंडिंग** किया जाता है। बैंकर्स राउंडिंग दो संख्याओं के बीच ठीक आधे रास्ते पर होने पर निकटतम सम संख्या के लिए गोल करती है, जो एक बड़े डेटासेट में गणनाओं में झुकाव को कम करती है और एक अधिक सांख्यिकीय महत्वपूर्ण परिणाम प्रदान करती है। हालाँकि, इससे उन लोगों के लिए अप्रत्याशित व्यवहार हो सकता है जो इससे परिचित नहीं हैं, विशेषकर जब हर मामले में पूर्ण सटीकता की अपेक्षा की जाती है।

इसके विपरीत, कई प्रोग्रामिंग भाषाओं और सिस्टम "गणितीय गोलाकार" या "हाफ-अप राउंडिंग" का उपयोग करते हैं, जहाँ दो संभावित गोल मानों के बीच बिल्कुल आधे रास्ते पर एक संख्या हमेशा ऊपर की ओर गोल की जाती है। VBA में अन्य भाषाओं से कोड का अनुवाद या पोर्टिंग करते समय, प्रोग्रामरों को वित्तीय और सांख्यिकीय अनुप्रयोगों में सूक्ष्म बग्स या अशुद्धियों से बचने के लिए इन भिन्नताओं को ध्यान में रखना होगा।

जबकि VBA गोलाकार के लिए कई प्रकार के फंक्शन प्रदान करता है, `Ceiling` और `Floor` फंक्शनों की अनुपस्थिति (बिना Excel के WorksheetFunction की शरण में जाए) इसकी मूल क्षमताओं में एक सीमा को उजागर करती है। अधिक सुविधा-संपन्न भाषाओं से आने वाले प्रोग्रामरों को ये चूकें असुविधाजनक लग सकती हैं और वे अपनी गणनाओं को उपलब्ध फंक्शनों का उपयोग करके अनुकूलित करने या कस्टम समाधान लागू करने के लिए मजबूर हो सकते हैं। फिर भी, VBA के गोलाकार फंक्शनों को सही ढंग से समझने और उपयोग करने से यह सुनिश्चित हो सकता है कि संख्यात्मक गणनाएँ सटीक हों और अधिकांश अनुप्रयोगों की आवश्यकताओं को पूरा करें।