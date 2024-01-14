---
title:    "C#: दो तारीखों की तुलना करना"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## क्यों:

कभी-कभी हमें दो तिथियों को तुलना करने की आवश्यकता होती है, जैसे कि कोई ईवेंट की तारीख से अगले साल की तारीख अनुमानित करना या दो आगामी घटनाओं के बीच अंतर निकालना। इसलिए हम आपको बताएंगे कि आप C# में दो तिथियों को कैसे तुलना कर सकते हैं। 

## कैसे: 

```C#
// दो तिथियों को मिलाने के लिए DateTime वेरिएबल बनाएं
DateTime date1 = new DateTime(2021, 10, 15);
DateTime date2 = new DateTime(2021, 10, 20);

// DateTime.Compare () में दो तिथियों को सिम्पल कम्पेयर और दो तिथियों को बड़ा या बड़ी कम्पेयर देखे
int compare = DateTime.Compare(date1, date2);
int compare2 = DateTime.Compare(date2, date1);

// सिम्पल कम्पेयर वापस -1 देगा अगर दो तिथियों में से एक पहले है और 0 यदि वे एक-सा हैं, और 1 यदि दोनों में से एक बड़ा है। 
Console.WriteLine("Date 1 compared to date 2: " + compare);
Console.WriteLine("Date 2 compared to date 1: " + compare2);

// बड़ा या बड़ी कम्पेयर वापस -1 देगा अगर दो तिथियों में से कोई एक से भी बड़ा है, 0 यदि वे एकसा हैं, और 1 यदि दोनों से कोई एक भी बड़ी है।
int compare3 = date1.CompareTo(date2);
int compare4 = date2.CompareTo(date1);

Console.WriteLine("Date 1 compared to date 2: " + compare3);
Console.WriteLine("Date 2 compared to date 1: " + compare4);
```

आउटपुट: 

Date 1 compared to date 2: -1
Date 2 compared to date 1: 1
Date 1 compared to date 2: -1
Date 2 compared to date 1: 1

## गहराई में जाएं: 

दो तिथियों को तुलना करने के लिए, सबसे पहले हम इसे DateTime वेरिएबल में लोड करते हैं। आप दो तिथियों के बीच अंतर निकालने के लिए इस्तेमाल कर सकते हैं DateTime.Subtract () विधि, जो दो DateTime वेरिएबल दो