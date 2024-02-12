---
title:                "एरर्स को हैंडल करना"
aliases:
- /hi/kotlin/handling-errors.md
date:                  2024-01-26T00:57:01.889051-07:00
model:                 gpt-4-1106-preview
simple_title:         "एरर्स को हैंडल करना"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/handling-errors.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
त्रुटियों को संभालना यह है कि आपका कोड क्रियान्वयन के दौरान उत्पन्न होने वाली समस्याओं से कैसे निपटता है - जैसे कि एक कर्वबॉल को बिना गिराए हुए संभालना। प्रोग्रामर इसे करते हैं ताकि क्रैश को रोक सकें और उपयोगकर्ताओं को एक सहज अनुभव प्रदान कर सकें।

## कैसे करें:
कोटलिन `try`, `catch`, `finally`, और `throw` प्रदान करता है त्रुटियों का प्रबंधन करने के लिए। यहाँ इनका प्रयोग कैसे करते हैं:

```Kotlin
fun main() {
    val numerator = 10
    val denominator = 0

    try {
        val result = numerator / denominator
        println("Result: $result")
    } catch (e: ArithmeticException) {
        println("Can't divide by zero, buddy.")
    } finally {
        println("This happens no matter what.")
    }
}
```

आउटपुट:
```
Can't divide by zero, buddy.
This happens no matter what.
```

यदि `try` ब्लॉक में कुछ गलत होता है, तो निष्पादन `catch` की ओर चला जाता है। यह विशेष त्रुटि को पकड़ता है (इस मामले में `ArithmeticException`)। `finally` ब्लॉक उसके बाद चलता है - किसी भी परिणाम के बावजूद।

## गहन अवलोकन
`try-catch` ब्लॉक शुरुआती प्रोग्रामिंग के दिनों से चला आ रहा है - यह एक सुरक्षा जाल की तरह है। कोटलिन `throw` भी प्रदान करता है जो मैन्युअली एक अपवाद को रिंग में फेंकने के लिए होता है, और `finally` कोड के लिए होता है जो चलना चाहिए - सफाई का काम, अक्सर।

विकल्पों में `Result` प्रकार शामिल है और कोटलिन का `try` एक अभिव्यक्ति के रूप में।

```Kotlin
val result: Result<Int> = try {
    Result.success(numerator / denominator)
} catch (e: ArithmeticException) {
    Result.failure(e)
}
```
यह दृष्टिकोण एक `Result` ऑब्जेक्ट लौटाता है - आपको या तो एक सफलता मिलती है या एक विफलता, अनुपचारित अपवाद के नाटक के बिना।

कोटलिन में कार्यान्वयन उत्तम है क्योंकि आप `try` का उपयोग एक अभिव्यक्ति के रूप में कर सकते हैं, इसका अर्थ है कि यह एक मूल्य लौटाता है। ऐसे विकल्प कोटलिन में त्रुटि संभालने को काफी बहुमुखी बनाते हैं। यह कार्यशाला में किसी साधन का सही उपयोग चुनने के बारे में है।

## यह भी देखें
- कोटलिन दस्तावेज़ पर अपवाद: [कोटलिन अपवाद संभाल](https://kotlinlang.org/docs/exception-handling.html)
- कोटलिन `Result` प्रकार दस्तावेज़: [कोटलिन रिजल्ट](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-result/)
- जोशुआ ब्लॉक द्वारा इफ़ेक्टिव जावा, तीसरा संस्करण - अपवादों पर शानदार अंतर्दृष्टि, हालांकि यह जावा-विशिष्ट है।
