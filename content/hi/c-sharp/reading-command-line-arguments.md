---
title:                "C#: समायोजन वाक्य तर्कों को पठन"
programming_language: "C#"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## क्यों

कभी-कभी हमें C# में प्रोग्रामिंग करते समय अपने प्रोग्राम को खरचों (arguments) से ज्यादा जानकारी उपलब्ध कराना पड़ता है। किसी भी प्रकार के कमांड लाइन खरचों को पढ़ने के लिए आपको कई विकल्प हो सकते हैं, और इस लेख के माध्यम से हम यह सीखेंगे कि आप ऐसे कई तरीके हैं कि कैसे हम आपके खरचों को प्रोग्राम में पढ़ सकते हैं। तो ज़रूर पढ़े कि आप आगे बढ़ सकें!

## कैसे करें

```C#
class Program
{
    static void Main(string[] args)
    {
        // सामान्य से प्रारंभ (start simple)
        Console.WriteLine("प्रोग्राम के यूजर को निम्नलिखित मदद मिलेगी।");
        Console.WriteLine("आप उन्हें यहाँ क्लिक करके कर सकते हैं:");
        Console.WriteLine("\n");

        // आप अनेक arguments को पढ़ सकते हैं:
        for (int i = 0; i < args.Length; i++)
        {
            Console.WriteLine($"खरच {i + 1} है: {args[i]}");
        }

        // ऐसा भी हो सकता है कि आप खरचों को ओवरराइड (override) कर सकें:
        // में आपको यहाँ एक वाईएररी (variable) बताऊंगा जो आपके क्लाइएंट (client) द्वारा
        // बहुत बार मार्शल (marshalled) होगा, और फिर अपना खुद का टाइप (type) बनाएगा:
        string name = args[0];
        Console.WriteLine("\n");
        Console.WriteLine($"नाम: {name}");
    }
}
```

जैसा कि आप देख सकते हैं, हमने कुछ सामान्य से प्रोग्राम लिखा है जो आपके खरचों को पढ़ेगा और आपको मदद करेगा इन्हें बनाने में! फिर आप ज़्यादा advanced code snippets को देखेंगे जो आपके क्लिए सरल हो सकते हैं।

## गहराईग़

कभी-कभी हमें ज़्यादा जानकारी की ज़रूरत होती है जब ह