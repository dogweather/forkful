---
title:    "C#: वर्तमान तारीख प्राप्त करना"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## क्यों

क्या आप भी निराश हो जाते हैं जब आपको कुछ कूड़ा भरने के लिए तारीख को गूगल करना पड़ता है? धैर्य रखें, आज मैं आपको इस समस्या का समाधान दूंगा। आइए अपने कंप्यूटर से वर्तमान तारीख को प्राप्त करने का बताता हूं।

## कैसे करें

अपने C# कोड में निम्न लाइन को शामिल करें:

```C#
DateTime today = DateTime.Now;
```

यह आपको अपने वर्तमान समय की प्रतिनिधि देता है। यदि आप तारीख, महीना या साल प्राप्त करना चाहते हैं, आप इसका उपयोग कर सकते हैं:

```C#
DateTime today = DateTime.Now;
int date = today.Day;
string month = today.ToString("MMMM");
int year = today.Year;
Console.WriteLine("आज " + month + " " + date + ", " + year + " है।");
```

आप इस कोड को अपने कंप्यूटर पर चलाकर वर्तमान तारीख के मजेदार हिसाब को देख सकते हैं।

## गहराई में उतरें

अब आप अपने सिस्टम समय के बारे में और गहराई से जानना चाहते हैं? धीरे से, शुरू से शुरू करें। Date and Time नामक प्रोपर्टी को DateTime कक्षा के साथ जोड़ा जाता है। DateTime.Now स्थानीय समय के साथ DateTime ऑब्जेक्ट बनाता है। आप अन्य प्रकार की तारीख के बारे में भी जान सकते हैं, जैसे DateTime.Now.ToLongDateString (), DateTime.Now.ToShortDateString (), और यूटिलिटी फ़ंक्शंस जैसे DateTime.Now.ToString("d"), DateTime.Now.ToString("D"), और DateTime.Now.ToString("MM/dd/yyyy")। इस संबंध में अधिक जानकारी के लिए, माइक्रोसॉफ्ट की आधिकारिक दस्तावेज़ीकरण पढ़ें।

## देखें भी

- [DateTime.Now.Property] (https://docs.microsoft.com/en-us/dotnet/api/system.datetime.now?view=net-5.0#properties)
- [DateTime Structure] (https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=