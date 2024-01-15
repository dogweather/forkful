---
title:                "Json से काम करना"
html_title:           "C#: Json से काम करना"
simple_title:         "Json से काम करना"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/working-with-json.md"
---

{{< edit_this_page >}}

## क्यों 

जावास्क्रिप्ट ऑब्जेक्ट नोटेशन (JSON) एक प्रसिद्ध डाटा इंटरचेंज फॉर्मेट है जो डेटा को स्टोर और एक्सचेंज करने के लिए इस्तेमाल किया जाता है। यह एक प्रभावी और आसान तरीका है डेटा को एक स्थिर स्ट्रक्चर में संग्रहीत करने का। इसलिए JSON को सीखने से आपको प्रोग्रामिंग और डेटा स्ट्रक्चर का अधिक ज्ञान होगा।

## कैसे करें 

```C#
// एक डिक्शनरी को JSON स्ट्रिंग में कन्वर्ट करने का उदाहरण
Dictionary<string, string> dictionary = new Dictionary<string, string>();
dictionary.Add("Name", "John Doe");
dictionary.Add("Age", "25");

string json = JsonConvert.SerializeObject(dictionary);
Console.WriteLine(json);

//output: {"Name": "John Doe", "Age": "25"}

// JSON स्ट्रिंग को डिक्शनरी में फिर से कन्वर्ट करने का उदाहरण
string json = "{\"Name\": \"John Doe\", \"Age\": \"25\"}";
Dictionary<string, string> dictionary = JsonConvert.DeserializeObject<Dictionary<string, string>>(json);

Console.WriteLine(dictionary["Name"]);
Console.WriteLine(dictionary["Age"]);

//output:John Doe
//25
```

## गहराई में जायें 

JSON अत्यंत उपयोगी हो सकता है जब आप अपने प्रोग्राम के साथ एक्सटर्नल API कॉल करते हैं और डाटा को प्राप्त करते हो। आप इसे अपने स्थानीय स्टोरेज से भी लोड कर सकते हैं। C# में JSON का उपयोग करना बहुत आसान है क्योंकि Newtonsoft.Json लाइब्रेरी हैंडलिंग परफेक्टमैंट के साथ JSON डेटा पर काम करती है। आशा है कि यह लेख आपको JSON के साथ काम करने में सहायता करेगा।

## इसे भी देखें 

- [JSON गाइड](https://docs.microsoft.com/en-us/dotnet/standard/serialization/system-text-json-how-to)
- [C# लाइब्रेरी उपयोग करने का तरीका](https://www.tutorialsteacher.com/csharp/csharp-json)
- [JSON कन्वर्टर उपयोग करना](https://www.dotnetperls.com/serialize-dictionary)