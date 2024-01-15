---
title:                "HTTP अनुरोध भेजना"
html_title:           "C#: HTTP अनुरोध भेजना"
simple_title:         "HTTP अनुरोध भेजना"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## क्यों

एचटीटीपी अनुरोध भेजना आमतौर पर वेब या मोबाइल ऐप्स के साथ संवाद स्थापित करने के लिए किया जाता है। इससे आप दूसरे सर्वर से डेटा या सेवाओं को प्राप्त कर सकते हैं और इस डेटा को अपने ऐप्स में उपयोग कर सकते हैं।

## कैसे करें

``` C#
// एचटीटीपी अनुरोध बनाएं
HttpWebRequest request = (HttpWebRequest)WebRequest.Create("यूआरएल");
// अनुरोध भेजें
HttpWebResponse response = (HttpWebResponse)request.GetResponse();
// उत्तर के साथ डेटा प्राप्त करें
Stream dataStream = response.GetResponseStream();
StreamReader reader = new StreamReader(dataStream);
string responseFromServer = reader.ReadToEnd();
Console.WriteLine(responseFromServer);
```

यहां "यूआरएल" को आपको भेजना होगा जहां से आप डेटा या सेवाओं को प्राप्त करना चाहते हैं। इसके बाद, आप उत्तर को पाठक से पढ़ सकते हैं और उसे अपने ऐप्स में उपयोग कर सकते हैं।

## गहराई में जाएं

HTTP यूआरएल के माध्यम से डेटा और सेवाओं को प्राप्त करने के लिए एक प्रमुख तरीका है। लेकिन आपको ध्यान रखने की जरूरत है कि अन्य कई तरीके भी हैं जिनके माध्यम से आप डेटा को प्राप्त कर सकते हैं, जैसे कि REST API या GraphQL। इन तरीकों को सीखने से आपके प्रोग्रामिंग कौशल को भी बढ़ावा मिलेगा।

## देखें भी

- [Microsoft दस्तावेज़](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpwebrequest?view=netcore-3.1) - एचटीटीपी अनुरोध से संबंधित अधिक जानकारी के लिए।
- [C# के साथ एचटीटीपी क्लाइंट लाइब्रेरी](https://www.nuget.org/packages/system.net.http/) - एचटीटीपी अनुरोध को भेजने के लिए हरमोनिक और एब