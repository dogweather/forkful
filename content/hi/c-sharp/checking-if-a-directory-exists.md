---
title:    "C#: डायरेक्टरी मौजूद है या नहीं जांचें"
keywords: ["C#"]
---

{{< edit_this_page >}}

## क्यों

डाइरेक्ट्री की जांच करने का उद्देश्य जानना है कि क्या एक पथ में इस डाइरेक्ट्री का होना हमारे काम के लिए आवश्यक है। इससे हम उपयोगकर्ता को दो समान प्रकार से मौजूदा फ़ाइलों में से एक को चुनने का विकल्प दे सकते हैं और एप्लिकेशन में कुछ सुरक्षा प्रश्नों को हल कर सकते हैं। ऐसी स्थिति में, डाइरेक्ट्री की जांच अनिवार्य हो जाती है।

## कैसे करें

```C#
if(Directory.Exists("C:\\Sample"))
{
    Console.WriteLine("Directory exists!");
}
else
{
    Console.WriteLine("Directory does not exist!");
}

// Output: Directory exists!
```

## गहराई में जाएं

डाइरेक्ट्री की जांच के पीछे आधुनिक कार्यप्रणाली का मूल सिद्धांत है। इससे आप न केवल एक साधारण सवाल का समाधान कर सकते हैं, बल्कि किसी बड़े साइज की डाइरेक्ट्री का मौजूदा अवस्था का भी पता लगा सकते हैं। आप अन्य सुरक्षा प्रश्नों को हल करने के लिए इसका उपयोग कर सकते हैं और समान नाम के कई फोल्डरों में भी अपना दिशा-निर्देश रख सकते हैं।

## देखें भी

- [Directory.Exists Method in C#](https://docs.microsoft.com/en-us/dotnet/api/system.io.directory.exists?view=net-5.0)
- [How to check if a directory exists in C#](https://www.c-sharpcorner.com/article/how-to-check-if-a-directory-exists-in-c-sharp/)