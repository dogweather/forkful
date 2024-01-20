---
title:                "भविष्य या अतीत में तारीख की गणना"
html_title:           "C#: भविष्य या अतीत में तारीख की गणना"
simple_title:         "भविष्य या अतीत में तारीख की गणना"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
दिनांक की गणना करना मतलब भूतकाल या भविष्य की तारीख का आकलन करना  होता है। यह उन समयों में होता है जब प्रोग्रामर्स को समय का प्रवाह ट्रैक करना पड़ता है, जैसे कि उत्पाद प्रलंबन की अवधि, या स्थायी उपयोगकर्ता बनाने के बाद कितना समय बीत गया। 

## कैसे:
यहाँ C# में भविष्य का दिन गणना करने का उदाहरण है:
```C#
DateTime today = DateTime.Today;// वर्तमान दिन 
DateTime futureDate = today.AddDays(5); //5 दिन अग्रिम
Console.WriteLine("आज की तारीख: {0}", today);
Console.WriteLine("भविष्य की तारीख: {0}", futureDate);
```
आउटपुट:
```
आज की तारीख: 2022-01-01
भविष्य की तारीख: 2022-01-06
```
## गहरा डाइव
(1) प्रोग्रामिंग भाषाओं ने समय और तारीख का ध्यान रखने वाली विशेषताएं बहुत पहले जोड़ दी थीं हालांकि C # का आगमन इसे और आसान बना देता है। (2) भविष्य या पिछले में तारीख की संगणना के विकल्प में DateTime और TimeSpan जैसी C# प्रकार शामिल हैं। (3) DateTime.Add विधियां हमें समय और तारीखों के निर्देशांकों को संशोधित करने की अनुमति देती है, बिना किसी स्थिर संख्या के संविधानीय अनुपात का पालन किए।

## अन्य स्रोतों का अवलोकन:
1. [Microsoft: DateTime Structure](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-5.0)
2. [Microsoft: TimeSpan Structure](https://docs.microsoft.com/en-us/dotnet/api/system.timespan?view=net-5.0)
3. [Stack Overflow: Calculate future dates](https://stackoverflow.com/questions/3784150/calculate-future-dates)