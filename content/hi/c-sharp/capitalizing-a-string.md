---
title:    "C#: स्ट्रिंग को सर्वाधिकृत करना"
keywords: ["C#"]
---

{{< edit_this_page >}}

## क्यों?

आपने कभी सोचा है कि स्ट्रिंग को capitalize कैसे किया जाता है? अक्सर हमें कोडिंग करते समय इस तरह की स्ट्रिंग्स का उपयोग करने की जरूरत पड़ती है। इस ब्लॉग पोस्ट में हम इस तरह की एक्टिविटी के बारे में बात करेंगे और सीखेंगे कि इसे कैसे किया जाता है।

## कैसे करें?

```C#
string text = "hello world";
string capitalizedText = text.ToUpper();
Console.WriteLine(capitalizedText);
```

एक simple technique स्ट्रिंग को capitalize करने के लिए `ToUpper()` function है। यह function स्ट्रिंग को uppercase में बदल देता है और उसे रिटर्न करता है। इस code का output नीचे दिया गया है।

```C#
HELLO WORLD
```

परंतु यदि आप एक समूह या फ्रेंड्स के नाम्स को capitalize करना चाहते हैं तो आपको एक अलग technique आवश्यक होगा। आप `FirstLetterToUpper()` फंक्शन का भी प्रयोग कर सकते हैं जो पहले शब्द के पहले अक्षर को uppercase में बदल देता है।

```C#
string name = "jane doe";
string capitalizedName = FirstLetterToUpper(name);

static string FirstLetterToUpper(string name)
{
    if(string.IsNullOrEmpty(name))
    {
        return string.Empty;
    }
    char[] letters = name.ToCharArray();
    letters[0] = char.ToUpper(letters[0]);
    return new string(letters);
}
```

इस code का output नीचे दिया गया है।

```C#
Jane doe
```

## गहराई में जाएं

स्ट्रिंग को capitalize करना थोड़ा complex हो सकता है यदि आप थोड़ा सा advanced code करना चाहते हैं। आप स्ट्रिंग के मूल्य को split कर और उसे समूहों में विभाजित कर सकते हैं। उदाहरण के लिए, यदि आप एक नाम को कैपिटलाइज करना चाहते हैं जैसे "john doe", तो आप इसे निम्न code से capitalize कर सकते हैं।

```C#
string name = "john doe";
string[] splittedName = name.split(' ');
name = FirstLetterToUpper(splittedName[0]) + " " + FirstLetterToUpper(splittedName[1]);
```

इस code का output नीचे दिया