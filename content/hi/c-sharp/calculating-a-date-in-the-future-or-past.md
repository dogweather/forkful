---
title:                "C#: भविष्य में या भूतकाल में एक दिनांक की गणना"
simple_title:         "भविष्य में या भूतकाल में एक दिनांक की गणना"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# क्यों

अगर आपको बिस्तर के साथ साल नए साल के बीच उस्ताद जानने की आवश्यकता होती है, तो आपको भविष्य या अतीत की तिथि की गणना करने की आवश्यकता हो सकती है।

# कैसे करें

```C#
// अगर आप उसी तारीख में से कुछ दिन जोड़ना चाहते हैं
DateTime futureDate = DateTime.Now.AddDays(5);
Console.WriteLine("आज के से 5 दिन बाद तिथि: {0}", futureDate.ToString("dd/MM/yyyy"));

// अगर आप एक विशेष तिथि के बाद के लिए तिथि की गणना करना चाहते हैं
DateTime pastDate = new DateTime(2021, 1, 1);
TimeSpan timePassed = DateTime.Now.Subtract(pastDate);
Console.WriteLine("1 जनवरी 2021 की गति से इस दिन तक: {0} दिन", timePassed.Days);
```

जैसा कि आप देख सकते हैं, हम `DateTime` और `TimeSpan` क्लास का उपयोग करके भविष्य और अतीत की तिथि की गणना कर सकते हैं। इन क्लासों में कई सुविधाएं हैं जो आपको तिथि, समय और अंतर की संभावना देती हैं।

# गहराई में जाओ

तिथि की गणना करना ऐसा काम है जो इसमें विभिन्न साल, महीने, सप्ताह और दिन की गणना करना हो सकता है। `DateTime` क्लास के साथ, आप अन्य महत्वपूर्ण तिथियों जैसे कि संभवता, दिन, मिनट, सेकंड और अंतर की गणना कर सकते हैं। इसके अलावा, आप `TimeSpan` क्लास का उपयोग करके दो तिथियों के बीच की अंतर भी जान सकते हैं।

# इससे अधिक देखें

[Microsoft दस्तावेज़](https://docs.microsoft.com/en-us/dotnet/standard/datetime/) | [DateTime कक्षा](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-5.0) | [TimeSpan कक्षा](https://docs.microsoft.com/en-us/dotnet/api/system.timespan?view=net-5.0)