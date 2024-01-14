---
title:                "C#: Json से काम करना"
simple_title:         "Json से काम करना"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/working-with-json.md"
---

{{< edit_this_page >}}

## क्यों

जेसोन एक प्रसिद्ध डेटा प्रारूप है जो आपको अपने एप्लिकेशन या वेबसाइट में डेटा को आसानी से संग्रहीत करने की अनुमति देता है। इसका उपयोग करके, आप जावास्क्रिप्ट ऑब्जेक्ट नोटेशन (JSON) के माध्यम से डेटा को समर्थित डेटा संरचनाओं में बदल सकते हैं जिससे डेटा प्रसंस्करण और डेटा प्रबंधन को आसानी से किया जा सकता है।

## कैसे करें

```C#
// एक साधारण JSON ऑब्जेक्ट बनाएं
string json = @"{
    ""name"": ""John Doe"",
    ""age"": 28,
    ""hobbies"": [""reading"", ""hiking"", ""cooking""]
}";

// JObject के रूप में ऑब्जेक्ट पढ़ें
JObject jobj = JObject.Parse(json);

// प्रत्येक विशेषता के लिए विशेषता को पढ़ें
string name = (string)jobj["name"];
int age = (int)jobj["age"];
JArray hobbies = (JArray)jobj["hobbies"];

// समस्त शब्दों को कॉन्सोल में प्रिंट करें
Console.WriteLine("नाम: " + name);
Console.WriteLine("आयु: " + age);
Console.Write("शौक: ");
foreach (string h in hobbies)
{
    Console.Write(h + ", ");
}
```

आप इस उदाहरण का उपयोग करके जेसोन ऑब्जेक्ट डेटा को आसानी से पढ़ सकते हैं और विशेषताओं को प्रसंस्करण कर सकते हैं। इसके अलावा, आप जेसोन को अपनी बिंदु-सारणी में भी बदल सकते हैं जिससे डेटा को अधिक अनुकूलित बनाया जा सकता है।

## गहराई में जाएं

JSON एक बहुत ही उपयोगी और गतिशील डेटा प्रारूप है जो डेटा संग्रहण और प्रसंस्करण को सीधा और आसान बनाता है। जबकि जेसोन प्रारूप अंतर्निहित रूप से सरल है, इसमें गहराई में जाने के साथ-साथ डेटा की सं