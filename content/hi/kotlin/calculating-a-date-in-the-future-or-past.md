---
title:                "Kotlin: भविष्य या भूतकाल में एक तिथि की गणना"
programming_language: "Kotlin"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# क्यों:

कभी-कभी हमारे कंप्यूटर या मोबाइल फोन में आज की तारीख से आगे और पीछे की तारीख को प्रिंट करने की आवश्यकता हो सकती है, जैसे कि किसी नयी मीटिंग के लिए एक दिन और समय का सामना करना हो। ऐसे मामलों में, हमारे पास अधिकांश तारीख कोड का समाधान नहीं होता है। इस मूल्यवान कौशल को सीखने से, हम ऐसी समस्याओं को आसानी से हल कर सकते हैं।

# कैसे करें:

```Kotlin
import java.time.format.DateTimeFormatter
import java.time.LocalDate

fun main() {
    val today = LocalDate.now()
    val futureDate = today.plusYears(1).plusMonths(2).plusDays(15)
    
    val formatter = DateTimeFormatter.ofPattern("dd MMMM yyyy")
    
    println("अगले वर्ष, दिसंबर के छह अंक्षर आठ वीं से अगले कुछ दिन की तारीख: ${futureDate.format(formatter)}")
}
```

उपरोक्त उदाहरण में, हमने `LocalDate` के सहायता से आज की तारीख को प्राप्त किया और उसे `plusYears()`, `plusMonths()` और `plusDays()` फंक्शन से आगे की तारीख को कैलकुलेट किया। इसके बाद, हमने तारीख को दिखने के लिए चाहे गए फॉर्मेट में स्ट्रिंग में रूपांतरित किया। फाइनली, हम उसे आसानी से प्रिंट कर सकते हैं।

# गहराई में जाएं:

आप अपने आगामी तारीख में से संभावित गुना-दशा, गुना-दहा, गुना-दोहरा आदि शामिल कर सकते हैं। आप भविष्य की कोई अन्य तारीख को भी गणना कर सकते हैं। `LocalDate` आपको अपने तारीख को कुछ हफ्तों, दिनों, महीनों या वर्षों बाद कलक्यूलेट करने की अनुमति देता है। आप इसे अन्य संघर्शों ज