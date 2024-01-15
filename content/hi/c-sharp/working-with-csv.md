---
title:                "कंप्यूटर प्रोग्रामिंग में csv का उपयोग करना"
html_title:           "C#: कंप्यूटर प्रोग्रामिंग में csv का उपयोग करना"
simple_title:         "कंप्यूटर प्रोग्रामिंग में csv का उपयोग करना"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/working-with-csv.md"
---

{{< edit_this_page >}}

## क्यों

CSV (Comma Separated Values) फ़ाइलें बड़ा साधन हैं जो डेटा संरचना को आसानी से परिभाषित करती हैं और कंप्यूटरों के बीच डेटा साझा करने में मदद करती हैं। यह संग्रहीत CSV फ़ाइलें अपनी संपत्ति, बिजनेस व्यवस्था और अन्य संग्रहण उपयोगों के लिए सबसे अधिक पसंद की जाती हैं।

## कैसे करें

अगर आप CSV फ़ाइलों के साथ काम करना सीखना चाहते हैं, तो हमारे द्वारा दिए गए कुछ आसान स्टेप्स का अनुसरण करें:

```C#
// CSV फ़ाइल कैसे पढ़ें
var csvData = File.ReadAllLines("example.csv");
foreach(var row in csvData)
{
    // अलग-अलग स्ट्रिंगों को पार्स करें
    string[] fields = row.Split(',');
    // दुगने की पूर्ति करें और स्क्रीन पर अंक दर्शाएँ
    Console.WriteLine(fields[0] * 2);
}

// CSV फ़ाइल में डेटा लिखें
List<string> data = new List<string> { "Name, Age, Occupation", "John, 28, Engineer", "Emily, 24, Teacher" };
File.WriteAllLines("example.csv", data);
```

## गहराई में जाएं

CSV फ़ाइलें बहुत सारे संरचनात्मक डेटा से बनाई जाती हैं जो कंप्यूटर अनुरूप होता है। इसलिए, ये मानक संरचना डेटा को दूसरे संग्रहीत सूचीबद्ध डेटा के साथ बहुत सुविधा से तुलना किया जा सकता है। C# के साथ CSV फ़ाइलों को पढ़ने और लिखने के लिए कई पुस्तकालय मौजूद हैं, जो आपको अपनी सुविधा से डेटा को पीछे से पढ़ने और स्वतंत्र डेटा को उत्कंठन करने की अनुमति देती हैं।

## देखें भी

- [C# मैं CSV फ़ाइलें कैसे पढ़ें और लिखें](https://www.geeksforgeeks