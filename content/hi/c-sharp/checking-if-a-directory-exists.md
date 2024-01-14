---
title:                "C#: डायरेक्टरी का अस्तित्व जांच करना"
simple_title:         "डायरेक्टरी का अस्तित्व जांच करना"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## क्यों

डायरेक्टरी मौजूद है की जांच कर न सिर्फ अपने कोड को सुरक्षित बनाने में मदद करता है, बल्कि यह भी जानने में मदद करता है कि आपके प्रोग्राम के साथ कौनसी फ़ोल्डर तक पहुंच सकते हैं।

## कैसे

एक डायरेक्टरी की उपस्थिति को जांचने के लिए, आपको `Directory.Exists()` मेथड को उपयोग करना होगा। नीचे दिए गए कोड ब्लॉक में आप इसका उदाहरण देख सकते हैं:

```C#
if (Directory.Exists(@"C:\Users\John\Documents"))
{
    Console.WriteLine("The directory exists!");
}
```

जब आप इस कोड ब्लॉक को अपने कंप्यूटर पर चलाएंगे, तो यदि फ़ोल्डर मौजूद होगा, तो आपको निम्न आउटपुट मिलेगा:

`The directory exists!`

## गहराई में प्रवेश करें

डायरेक्टरी की उपस्थिति की जांच के पीछे के हरे भाग में, कई विभिन्न प्रकार के कारण हो सकते हैं। ये कारण अनेक तरह के हो सकते हैं, जैसे कि फ़ाइल पाठ में त्रुटि या फ़ोल्डर की सेवा के लिए अनुमति न होना। आप `Directory.Exists()` मेथड को उपयोग करने से पहले इन सभी कारणों को ध्यान में रख सकते हैं। इसके अलावा, आप फ़ोल्डर की शक्ति को दोबारा हासिल करने के लिए `Directory.GetAccessControl()` मेथड को उपयोग कर सकते हैं।

## देखें भी

- [C# में फ़ाइलें और डायरेक्टरियों को पढ़ें](https://docs.microsoft.com/en-us/dotnet/standard/io/how-to-iterate-through-a-directory-tree)
- [C# में डायरेक्टरी बनाएं और हटाएं](https://docs.microsoft.com/en-us/dotnet/standard/io/how-to-create-and-delete-a-directory)
- [C# में फ़ाइलों और फ़ोल्डरों को कै