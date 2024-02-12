---
title:                "रिफैक्टरिंग"
aliases:
- /hi/c-sharp/refactoring.md
date:                  2024-01-26T01:29:12.559687-07:00
model:                 gpt-4-0125-preview
simple_title:         "रिफैक्टरिंग"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/refactoring.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

रिफैक्टरिंग उस प्रक्रिया को कहते हैं जिसमें मौजूदा कंप्यूटर कोड को पुन: संरचित किया जाता है बिना इसके बाह्य व्यवहार को बदले। प्रोग्रामर्स इसे कोड को साफ करने, पठनीयता में वृद्धि, जटिलता को कम करने, और रखरखाव में सुधार के लिए करते हैं।

## कैसे करें:

आइए एक सरल C# विधि को रिफैक्टर करते हैं जो किसी संख्याओं के ऐरे का योग गणना करती है और प्रिंट करती है:

रिफैक्टरिंग से पहले:
```C#
public class Calculator
{
    public void CalculateSum()
    {
        int[] numbers = { 1, 2, 3, 4, 5 };
        int sum = 0;
        for (int i = 0; i < numbers.Length; i++)
        {
            sum += numbers[i];
        }
        Console.WriteLine("The sum is " + sum);
    }
}
```

रिफैक्टरिंग के बाद:
```C#
public class Calculator
{
    private readonly int[] _numbers;

    public Calculator(int[] numbers)
    {
        _numbers = numbers;
    }

    public int CalculateSum()
    {
        return _numbers.Sum();
    }

    public void DisplaySum()
    {
        Console.WriteLine($"The sum is {CalculateSum()}");
    }
}

// उपयोग:
var calculator = new Calculator(new[] { 1, 2, 3, 4, 5 });
calculator.DisplaySum();
```

रिफैक्टरिंग करके, हमने चिंताओं को अलग किया है, `Calculator` क्लास को अधिक लचीला बनाया है जिससे यह किसी भी संख्या ऐरे को स्वीकार कर सके, और LINQ का उपयोग करके योग गणना को अधिक संक्षिप्त बनाया है।

## गहराई में जानें

रिफैक्टरिंग की जड़ें स्मॉलटॉक प्रोग्रामिंग समुदाय में हैं और 1990 के दशक में मार्टिन फाउलर की पुस्तक "रिफैक्टरिंग: इम्प्रूविंग द डिज़ाइन ऑफ़ एग्ज़िस्टिंग कोड" द्वारा लोकप्रिय हुई। वर्षों में, यह एजाइल पद्धतियों और अच्छे कोडिंग प्रथाओं का एक मूलभूत हिस्सा बन गई है।

रिफैक्टरिंग के विभिन्न दृष्टिकोण होते हैं, जैसे कि टेस्ट-ड्राइवेन डेवलपमेंट (TDD) में रेड-ग्रीन-रिफैक्टर। यह सुनिश्चित करता है कि रिफैक्टरिंग बग्स को पेश न करे, एक असफल परीक्षण के साथ शुरू करके, इसे पास करने, और फिर कोड को साफ करने।

रिफैक्टरिंग लागू करते समय, प्रक्रिया के दौरान कोई भी कार्यक्षमता टूटने न पाए, इसके लिए एक व्यापक परीक्षण सूट होना महत्वपूर्ण है। ऑटोमेटेड रिफैक्टरिंग उपकरण, जैसे कि C# के लिए ReSharper, इस प्रक्रिया में सहायता कर सकते हैं जो सुरक्षित तरीके से कोड संरचनाओं को बदलने में मदद करते हैं। हालांकि, उपकरणों को कोडबेस और कोडिंग सिद्धांतों की गहरी समझ के पूरक होना चाहिए।

## देखें भी

- मार्टिन फाउलर की अग्रणी कृति रिफैक्टरिंग पर: [Refactoring: Improving the Design of Existing Code](https://martinfowler.com/books/refactoring.html)
- विजुअल स्टूडियो में रिफैक्टरिंग पर माइक्रोसॉफ्ट की गाइड: [Refactoring (C#)](https://docs.microsoft.com/en-us/visualstudio/ide/refactoring-in-visual-studio?view=vs-2022)
- उदाहरणों के साथ रिफैक्टरिंग पैटर्न में विस्तृत नज़रिया: [SourceMaking Refactoring](https://sourcemaking.com/refactoring)
